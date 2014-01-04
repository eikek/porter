package porter.app.openid.routes

import spray.routing.{Directives, Directive0, Route}
import porter.model.{Account, Credentials}
import porter.app.openid.common.{LocalId, Keys}

trait EndpointRoute extends Directives with AuthDirectives with PageDirectives {
  self: OpenIdActors =>

  import _root_.porter.app.akka.Porter.Messages.store._
  import _root_.porter.app.akka.Porter.Messages.mutableStore._
  import akka.pattern.ask
  import Implicits._

  private def continueRemembered(account: Account, params: Map[String, String]): Directive0 = {
    params.get(Keys.realm.openid) match {
      case Some(or) =>
        if (rememberRealmProperty(or).isTrue(account.props)) pass
        else reject()
      case None => reject()
    }
  }

  private def updateContinueRemembered(localId: LocalId, realmUri: String) {
    val f = for {
      acc <- (porterRef ? FindAccounts(localId.realm, Set(localId.account))).mapTo[FindAccountsResp]
      if acc.accounts.nonEmpty
      upd <- (porterRef ? UpdateAccount(localId.realm, acc.accounts.head.updatedProps(rememberRealmProperty(realmUri).set(true)))).mapTo[OperationFinished]
    } yield upd
    f onFailure { case x =>
      log.error(x, "Unable to remember continue decision")
    }
  }

  private def authenticateRoute(creds: Set[Credentials]): Route = {
    extractRealm { realm =>
      authcToken(realm, creds)(timeout) { auth =>
        positiveAssertion(auth, realm) { params =>
          setPorterCookieOnRememberme(auth.account) {
            isSetup {
              continueRemembered(auth.account, params) {
                redirectToRelyingParty(params)
              } ~
              renderContinuePage(params)
            } ~
            isImmediate {
              redirectToRelyingParty(params)
            }
          }
        }
      } ~
      renderLoginPage(settings.endpointUrl.path.toString(), failed = true)
    }
  }

  def checkRoute: Route = {
    path(openIdEndpoint) {
      isAssociate {
        createAssociation ~
          complete(errorResponse(direct = true, "Invalid association request"))
      } ~
      isCheckAuth {
        lookupAssociation(_.priv)(timeout) { assoc =>
          allParams { params =>
            val valid = validateResponse(assoc.token, params)
            complete(checkAssocResponse(valid, assoc.handle))
          }
        } ~
        complete(errorResponse(direct = true, "Invalid check-auth request"))
      } ~
      isImmediate {
        noCredentials {
          complete(setupNeededResponse)
        } ~
        credentials { creds =>
          authenticateRoute(creds) ~
            complete(errorResponse(direct = true, "Invalid immediate checkid request"))
        }
      } ~
      isSetup {
        signinCancel {
          redirectToRelyingParty(userCancelResponse)
        } ~
        noCredentials {
          renderLoginPage(settings.endpointUrl.path.toString(), failed = false)
        } ~
        credentials { creds =>
          authenticateRoute(creds) ~
          redirectToRelyingParty(errorResponse(direct = false, "Invalid checkid request"))
        }
      } ~
      continueSubmit {
        allParams { params =>
          if (params.get("porter.rememberContinue").exists(_.toLowerCase == "on")) {
            (params.get("porter.account"), params.get("porter.realm"), params.get(Keys.realm.openid)) match {
              case (Some(a), Some(r), Some(uri)) => updateContinueRemembered(LocalId(r, a), uri)
              case _ =>
            }
          }
          redirectToRelyingParty(params.filter({ case (k, v) => !k.startsWith("porter.") }))
        }
      } ~
      renderErrorPage
    }
  }
}
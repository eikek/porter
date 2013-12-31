package porter.app.openid.routes

import spray.routing.{Directive0, Route}
import porter.model.{Account, Credentials}
import porter.app.openid.common.{LocalId, Keys}
import porter.util.Hash

trait EndpointRoute {
  self: AuthDirectives with PageDirectives =>

  import spray.routing.Directives._
  import _root_.porter.model.Property._
  import _root_.porter.app.akka.Porter.Messages.store._
  import _root_.porter.app.akka.Porter.Messages.mutableStore._
  import akka.pattern.ask


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
      acc <- (porter ? FindAccounts(localId.realm, Set(localId.account))).mapTo[FindAccountsResp]
      if acc.accounts.nonEmpty
      upd <- (porter ? UpdateAccount(localId.realm, acc.accounts.head.updatedProps(rememberRealmProperty(realmUri).set(true)))).mapTo[OperationFinished]
    } yield upd
    f onFailure { case x =>
      log.error(x, "Unable to remember continue decision")
    }
  }

  private def authenticateRoute(creds: Set[Credentials]): Route = {
    extractRealm { realm =>
      authenticateToken(creds, realm)(timeout) { auth =>
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
      renderLoginPage(failed = true)
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
          renderLoginPage(failed = false)
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
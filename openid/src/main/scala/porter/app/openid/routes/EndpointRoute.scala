/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porter.app.openid.routes

import spray.routing.{Directives, Directive0, Route}
import porter.model.{Ident, Account, Credentials}
import porter.app.openid.common.{SRegKeys, Authenticated, LocalId, Keys}

trait EndpointRoute extends Directives with AuthDirectives with PageDirectives {
  self: OpenIdActors =>

  import _root_.porter.app.akka.Porter.Messages.store._
  import _root_.porter.app.akka.Porter.Messages.mutableStore._
  import akka.pattern.ask
  import Implicits._

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
          redirectToRelyingParty(setupNeededResponse)
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
          renderLoginPage(settings.endpointUrl, failed = false)
        } ~
        credentials { creds =>
          authenticateRoute(creds) ~
            redirectToRelyingParty(errorResponse(direct = false, "Invalid checkid request"))
        }
      } ~
      renderErrorPage
    }
  }

  private def authenticateRoute(creds: Set[Credentials]): Route = {
    extractRealm { realm =>
      authcAccount(realm, creds)(timeout) { account =>
        setPorterCookieOnRememberme(account) {
          returnImmediately(account) {
            sendOpenIdAssertion(account, realm)
          } ~
          continueSubmit {
            rememberContinueSubmit {
              sendOpenIdAssertion(account, realm)
            }
          } ~
          allParams { params =>
            renderContinuePage(account, params)
          }
        }
      } ~
      renderLoginPage(settings.endpointUrl, failed = true)
    }
  }

  /**
   * Sends the final assertion to the client.
   * @param account
   * @param realm
   * @return
   */
  private def sendOpenIdAssertion(account: Account, realm: Ident) = {
    anyParam(Keys.assoc_handle.openid.?) { handle =>
      association(handle)(timeout) { assoc =>
        positiveAssertion(Authenticated(account, assoc), realm) { resp =>
          redirectToRelyingParty(resp)
        }
      }
    }
  }

  /**
   * Passes, if the realm of the current openid request is remembered
   * in the account properties
   * @param account
   * @return
   */
  private def isContinueRemembered(account: Account): Directive0 = {
    param(Keys.realm.openid).flatMap { realmuri =>
      if (rememberRealmProperty(realmuri).isTrue(account.props)) pass
      else reject()
    }
  }

  /**
   * Passes, if the openid request does not carry an extension request
   * @return
   */
  private def nonExtension: Directive0 = {
    allParams.flatMap { map => 
      if (map.contains(SRegKeys.required.openid) || map.contains(SRegKeys.optional.openid)) reject()
      else pass
    }
  }

  /**
   * Starts a future that will store the given realm-uri into the
   * properties of the given account
   * @param localId
   * @param realmUri
   */
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

  /**
   * Always passes and starts an operation before, that will remember the
   * realm of the current relying party, if the user has chosen so.
   * @return
   */
  private def rememberContinueSubmit: Directive0 = {
    allParams.flatMap { params =>
      if (params.get("porter.rememberContinue").exists(_.toLowerCase == "on")) {
        (params.get("porter.account"), params.get("porter.realm"), params.get(Keys.realm.openid)) match {
          case (Some(a), Some(r), Some(uri)) => updateContinueRemembered(LocalId(r, a), uri)
          case _ =>
        }
      }
      pass
    }
  }

  /**
   * Passes, if the request should be answered without user interaction
   * @param account
   * @return
   */
  private def returnImmediately(account: Account) = {
    isImmediate | (nonExtension & isContinueRemembered(account))
  }

  
}
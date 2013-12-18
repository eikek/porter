package porter.app.openid.routes

import spray.routing.Route

trait EndpointRoute {
  self: AuthDirectives with PageDirectives =>

  import spray.routing.Directives._

  def checkRoute: Route = {
    path(openIdEndpoint) {
      isAssociate {
        createAssociation
      } ~
      isCheckAuth {
        associationToken(_.priv)(timeout) { assoc =>
          allParams { params =>
            val valid = validateResponse(assoc.token, params)
            complete(checkAssocResponse(valid, assoc.handle))
          }
        }
      } ~
      (isImmediate & noCredentials) {
        redirectToRelyingParty(setupNeededResponse)
      } ~
      (isSetup | isImmediate) {
        userCancel {
          redirectToRelyingParty(userCancelResponse)
        } ~
        noCredentials {
          renderLoginPage(failed = false)
        } ~
        credentials { creds =>
          extractRealm { realm =>
            authenticateToken(creds, realm)(timeout) { auth =>
              positiveAssertion(auth, realm) { params =>
                setPorterCookie(auth.account) {
                  isSetup {
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
      }
    }
  }
}

package porter.app.openid.routes

import spray.routing.Route
import porter.model.Credentials

trait EndpointRoute {
  self: AuthDirectives with PageDirectives =>

  import spray.routing.Directives._

  private def authenticateRoute(creds: Set[Credentials]): Route = {
    extractRealm { realm =>
      authenticateToken(creds, realm)(timeout) { auth =>
        positiveAssertion(auth, realm) { params =>
          setPorterCookieOnRememberme(auth.account) {
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
        userCancel {
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
      renderErrorPage
    }
  }
}

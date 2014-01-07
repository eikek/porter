package porter.app.akka.http

import akka.actor.ActorRef
import spray.routing.{Route, Directives}
import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.app.akka.Porter.Messages.authz._
import porter.app.akka.Porter.Messages.authc._
import porter.app.akka.Porter.Messages.mutableStore._
import porter.app.akka.api.PorterMain.UpdateAuthProps
import porter.auth.{OneSuccessfulVote, Decider}
import porter.app.akka.PorterUtil
import porter.client.Messages.auth.AuthAccount

class AuthService(porterRef: ActorRef, decider: Decider = OneSuccessfulVote)(implicit ec: ExecutionContext, to: Timeout) extends Directives {
  import PorterJsonProtocol._
  import spray.httpx.SprayJsonSupport._
  import akka.pattern.ask

  def ruleList(policy: porter.model.Policy): List[String] =
    policy.permissions.map(_.toString).toList ::: policy.revocations.map(_.toString).toList

  def route: Route = {
    path("api" / "authz") {
      post {
        handleWith { req: Authorize =>
          (porterRef ? req).mapTo[AuthorizeResp]
        }
      }
    } ~
    path("api" / "policy") {
      post {
        handleWith { req: GetPolicy =>
          (porterRef ? req).mapTo[GetPolicyResp]
            .map(gpr => JsPolicyResp(gpr.account, ruleList(gpr.policy)))
        }
      }
    } ~
    path("api" / "authc") {
      post {
        handleWith { req: Authenticate =>
          (porterRef ? req).mapTo[AuthenticateResp]
        }
      }
    } ~
    path("api" / "authcAccount") {
      post {
        handleWith { req: Authenticate =>
          PorterUtil.authenticateAccount(porterRef, req.realmId, req.creds, decider)
            .map(t => AuthAccount(success = true, Some(t._2)))
            .recover({ case x => AuthAccount(success = false, None) })
        }
      }
    } ~
    path("api" / "updateAuthProps") {
      post {
        handleWith { req: UpdateAuthProps =>
          (porterRef ? req).mapTo[OperationFinished]
        }
      }
    }
  }
}

object AuthService {
  def apply(porter: ActorRef, decider: Decider = OneSuccessfulVote)
           (implicit ec: ExecutionContext, to: Timeout): AuthService = new AuthService(porter)
}
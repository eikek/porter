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

package porter.app.akka.http

import akka.actor.ActorRef
import spray.routing.{Route, Directives}
import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.app.akka.Porter.Messages.authz._
import porter.app.akka.Porter.Messages.authc._
import porter.auth.{OneSuccessfulVote, Decider}
import porter.app.akka.PorterUtil
import porter.client.Messages.auth.{RetrieveServerNonceResp, RetrieveServerNonce, AuthAccount}
import porter.app.client.PorterAkkaClient
import porter.model.PasswordCrypt

class AuthService(client: PorterAkkaClient)(implicit ec: ExecutionContext, to: Timeout) extends Directives {
  import PorterJsonProtocol._
  import spray.httpx.SprayJsonSupport._
  import akka.pattern.ask

  private implicit val timeout = to.duration

  def ruleList(policy: porter.model.Policy): List[String] =
    policy.permissions.map(_.toString).toList ::: policy.revocations.map(_.toString).toList

  def route: Route = {
    path("api" / "authz") {
      post {
        handleWith { req: Authorize =>
          client.authorize(req)
        }
      }
    } ~
    path("api" / "authz" / "policy") {
      post {
        handleWith { req: GetPolicy =>
          (client.porterRef ? req).mapTo[GetPolicyResp]
            .map(gpr => JsPolicyResp(gpr.account, ruleList(gpr.policy)))
        }
      }
    } ~
    path("api" / "authc") {
      post {
        handleWith { req: Authenticate =>
          client.authenticate(req)
        }
      }
    } ~
    path("api" / "authc" / "account") {
      post {
        handleWith { req: Authenticate =>
          client.authenticateAccount(req)
        }
      }
    } ~
    path("api" / "authc" / "serverNonce") {
      handleWith { req: RetrieveServerNonce =>
        client.retrieveNonce(req)
      }
    }
  }
}

object AuthService {
  def apply(porter: ActorRef, decider: Decider = OneSuccessfulVote)
           (implicit ec: ExecutionContext, to: Timeout): AuthService =
    new AuthService(new PorterAkkaClient(porter, decider, PasswordCrypt.randomCrypt))
}
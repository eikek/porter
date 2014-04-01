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

package porter.app.client.spray

import spray.routing.authentication._
import scala.concurrent.{Future, ExecutionContext, Promise}
import porter.auth.{OneSuccessfulVote, Decider}
import porter.model.{Credentials, Ident, Account}
import spray.routing.{StandardRoute, RequestContext}
import akka.actor.ActorRef
import akka.util.Timeout
import spray.routing.AuthenticationFailedRejection.CredentialsRejected
import porter.app.akka.{PorterRef, PorterUtil}

class PorterAuthenticator(porterContext: PorterContext,
                          cookieKey: Vector[Byte],
                          cookieName: String = "PORTER")(implicit ec: ExecutionContext, to: Timeout)
  extends ContextAuthenticator[Account] {

  import PorterDirectives._
  import PorterAuthenticator._

  def apply(context: RequestContext) = {
    val promise = Promise[Authentication[Account]]()
    val route = allCredentials(cookieKey, cookieName).happly(creds => new StandardRoute {
      def apply(ctx: RequestContext) = {
        val f = authenticate(porterContext.porterRef, porterContext.realm, creds.head, porterContext.decider)
        f.onComplete({ case r => promise.complete(r) })
      }
    })
    route(context)
    promise.future
  }
}

object PorterAuthenticator {
  import PorterDirectives._

  def apply(porterContext: PorterContext,
            cookieKey: Vector[Byte],
            cookieName: String = "PORTER")(implicit ec: ExecutionContext, to: Timeout) =
    new PorterAuthenticator(porterContext, cookieKey, cookieName)

  /**
   * Authenticates a user and provides the [[porter.model.Account]] if authentication was
   * successful. Otherwise a http header is created to request http basic credentials from
   * the client.
   *
   * @param porter
   * @param realm
   * @param creds
   * @param decider
   * @param ec
   * @param timeout
   * @return
   */
  def authenticate(porter: PorterRef,
                   realm: Ident,
                   creds: Set[Credentials],
                   decider: Decider = OneSuccessfulVote)(implicit ec: ExecutionContext, timeout: Timeout): Future[Authentication[Account]] = {
    PorterUtil.authenticateAccount(porter, realm, creds, decider).map(_._2).map(Right.apply).recover {
      case ex => Left(httpBasicChallenge(CredentialsRejected, realm.name))
    }
  }
}
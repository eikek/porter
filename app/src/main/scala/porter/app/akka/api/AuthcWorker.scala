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

package porter.app.akka.api

import akka.actor._
import porter.auth._
import porter.auth.AuthToken
import porter.model.{AccountCredentials, PasswordCredentials, Account}
import scala.Some

/**
 * Actor for processing one `Authenticate` request. After done, the actor stops itself.
 *
 */
class AuthcWorker(store: ActorRef, validators: List[Validator]) extends Actor with ActorLogging {
  import AuthcWorker._
  import porter.client.messages._
  import StoreActor._

  val handlers = validators.zipWithIndex.map { case (a,i) =>
    context.actorOf(handlerProps(a), name = s"validator$i")
  }

  if (handlers.isEmpty) context.become(empty)

  def receive = normal

  def empty: Receive = {
    case req@Authenticate(realm, creds) =>
      sender ! Status.Failure(new Exception("No validators defined."))
  }

  def normal: Receive = {
    case req@Authenticate(realm, creds) =>
      store ! FindRealms(Set(realm))
      store ! FindAccountsFor(realm, creds)
      context.become(waiting(sender, None, None, req))
  }

  private case object Done

  def waiting(client: ActorRef, r: Option[FindRealmsResp], a: Option[FindAccountsResp], req: Authenticate): Receive = {
    case m: FindRealmsResp =>
      log.debug(s"Found realms ${m.realms}")
      context.become(waiting(client, Some(m), a, req))
      if (a.nonEmpty) self ! Done

    case m: FindAccountsResp =>
      log.debug(s"Found accounts ${m.accounts}")
      context.become(waiting(client, r, Some(m), req))
      if (r.nonEmpty) self ! Done

    case err: Status.Failure =>
      log.error(err.cause, "Received status failure while authenticating")
      client ! err

    case Done =>
      if (r.isEmpty || a.isEmpty) {
        log.error("Internal error: responses not ready.")
        client ! Status.Failure(new Exception("internal error: responses not ready"))
        context.stop(self)
      } else {
        (r.get.realms.headOption, extractAccount(req, a.get)) match {
          case (Some(realm), Some(acc)) =>
            val token = AuthToken(realm, acc, req.creds)
            if (handlers.isEmpty) {
              log.debug(s"No handlers! Send authc response ${token.toResult}")
              client ! AuthenticateResp(Some(token.toResult))
              context.stop(self)
            } else {
              handlers.foreach(_ ! token)
              context.become(authenticating(client, token, req, handlers.toSet))
            }
          case _ =>
            client ! AuthenticateResp(None)
            context.stop(self)
        }
      }
  }

  private def extractAccount(req: Authenticate, resp: FindAccountsResp): Option[Account] =
    resp.accounts.headOption.orElse {
      val name = req.creds.collect({ case pw: AccountCredentials => pw.accountName.name})
      name.headOption.map(Account.apply(_))
    }

  def authenticating(client: ActorRef, token: AuthToken, req: Authenticate, refs: Set[ActorRef]): Receive = {
    case t:AuthToken if refs.size <= 1 =>
      val merged = merge(token, t)
      log.debug(s"Send authc response ${merged.toResult}")
      client ! AuthenticateResp(Some(merged.toResult))
      context.stop(self)

    case t:AuthToken if refs.size > 1 =>
      context.become(authenticating(client, merge(token, t), req, refs - sender))
  }
}

object AuthcWorker {

  def apply(store: ActorRef, handlers: Iterable[Validator]) =
    Props(classOf[AuthcWorker], store, handlers)

  def handlerProps(auth: Validator) = Props(classOf[HandlerActor], auth)

  class HandlerActor(auth: Validator) extends Actor {
    def receive = {
      case token: AuthToken => sender ! auth.authenticate(token)
    }
  }

  private def merge(t1: AuthToken, t2: AuthToken): AuthToken = {
    val newmap = t1.votes.foldLeft(t2.votes) { case (m, (id, v)) =>
      if (v.isFailed) m.updated(id, v)
      else m.get(id) match {
        case Some(ev) if ev.isFailed => m
        case Some(ev) if ev.isSuccess => m.updated(id, v)
        case None => m.updated(id, v)
      }
    }
    t1.copy(votes = newmap)
  }
}

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

import akka.actor.{Status, Props, ActorRef, Actor}
import porter.auth._

/**
 * Actor for processing one `Authenticate` request. After done, the actor stops itself.
 *
 */
class AuthcWorker(store: ActorRef, validators: List[Validator]) extends Actor {
  import AuthcWorker._
  import porter.client.Messages.auth._
  import StoreActor.messages._

  val handlers = validators.map(a => context.actorOf(handlerProps(a)))

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
      context.become(waiting(client, Some(m), a, req))
      if (a.nonEmpty) self ! Done

    case m: FindAccountsResp =>
      context.become(waiting(client, r, Some(m), req))
      if (r.nonEmpty) self ! Done

    case err: Status.Failure =>
      client ! err

    case Done =>
      if (r.isEmpty || a.isEmpty) {
        client ! Status.Failure(new Exception("internal error: responses not ready"))
        context.stop(self)
      } else {
        (r.get.realms.headOption, a.get.accounts.headOption) match {
          case (Some(realm), Some(acc)) =>
            val token = AuthToken(realm, acc, req.creds)
            if (handlers.isEmpty) {
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

  def authenticating(client: ActorRef, token: AuthToken, req: Authenticate, refs: Set[ActorRef]): Receive = {
    case t:AuthToken if refs.size <= 1 =>
      val merged = merge(token, t)
      client ! AuthenticateResp(Some(merged.toResult))
      context.stop(self)

    case t:AuthToken if refs.size > 1 =>
      context.become(authenticating(client, merge(token, t), req, refs - sender))
  }
}

object AuthcWorker {
  object messages {
    type Authenticate = porter.client.Messages.auth.Authenticate
    val Authenticate = porter.client.Messages.auth.Authenticate

    type AuthenticateResp = porter.client.Messages.auth.AuthenticateResp
    val AuthenticateResp = porter.client.Messages.auth.AuthenticateResp
  }

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

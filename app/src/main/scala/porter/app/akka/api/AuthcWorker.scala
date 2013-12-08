package porter.app.akka.api

import akka.actor.{Status, Props, ActorRef, Actor}
import porter.auth._
import porter.auth.AuthResult
import porter.auth.AuthToken
import scala.Some

/**
 * Actor for processing one `Authenticate` request. After done, the actor stops itself.
 *
 * @since 05.12.13 23:24
 */
class AuthcWorker(store: ActorRef, authenticators: List[Authenticator]) extends Actor {
  import AuthcWorker._
  import AuthcWorker.messages._
  import StoreActor.messages._

  val handlers = authenticators.map(a => context.actorOf(handlerProps(a)))

  def receive = normal

  def normal: Receive = {
    case req@Authenticate(realm, creds, id) =>
      store ! FindRealms(Set(realm), 1)
      store ! FindAccountsFor(realm, creds, 2)
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

    case Done =>
      if (r.isEmpty || a.isEmpty) {
        client ! Status.Failure(new Exception("internal error: responses not ready"))
        context.stop(self)
      } else {
        (r.get.realms.headOption, a.get.accounts.headOption) match {
          case (Some(realm), Some(acc)) =>
            val token = AuthToken(realm, acc, req.creds)
            if (handlers.isEmpty) {
              client ! AuthenticateResp(Some(token.toResult), req.id)
              context.stop(self)
            } else {
              handlers.foreach(_ ! token)
              context.become(authenticating(client, token, req, handlers.toSet))
            }
          case _ =>
            client ! AuthenticateResp(None, req.id)
            context.stop(self)
        }
      }
  }

  def authenticating(client: ActorRef, token: AuthToken, req: Authenticate, refs: Set[ActorRef]): Receive = {
    case t:AuthToken if refs.size <= 1 =>
      val merged = merge(token, t)
      client ! AuthenticateResp(Some(merged.toResult), req.id)
      context.stop(self)

    case t:AuthToken if refs.size > 1 =>
      context.become(authenticating(client, merge(token, t), req, refs - sender))
  }
}

object AuthcWorker {
  import porter.model._

  def apply(store: ActorRef, handlers: Iterable[Authenticator]) =
    Props(classOf[AuthcWorker], store, handlers)

  def handlerProps(auth: Authenticator) = Props(classOf[HandlerActor], auth)

  class HandlerActor(auth: Authenticator) extends Actor {
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

  object messages {
    case class Authenticate(realmId: Ident, creds: Set[Credentials], id: Int = 0) extends RealmMessage
    case class AuthenticateResp(result: Option[AuthResult], id: Int) extends PorterMessage
  }
}

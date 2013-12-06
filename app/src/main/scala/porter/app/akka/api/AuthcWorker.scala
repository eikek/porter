package porter.app.akka.api

import akka.actor.{Props, ActorRef, Actor}
import porter.auth._
import porter.auth.AuthResult
import porter.app.akka.api.StoreActor.FindRealmsResponse
import porter.app.akka.api.StoreActor.FindAccountsResp
import porter.auth.AuthToken
import scala.Some

/**
 * Actor for processing one `Authenticate` request. After done, the actor stops itself.
 *
 * @since 05.12.13 23:24
 */
class AuthcWorker(store: ActorRef, authenticators: List[Authenticator]) extends Actor {
  import AuthcWorker._
  import porter.model._

  val handlers = authenticators.map(a => context.actorOf(handlerProps(a)))

  def receive = normal

  def normal: Receive = {
    case req@Authenticate(realm, creds, id) =>
      store ! StoreActor.FindRealms(Set(realm), 1)
      store ! StoreActor.FindAccountsFor(realm, creds, 2)
      context.become(waiting(sender, None, None, req))
  }

  def waiting(client: ActorRef, r: Option[FindRealmsResponse], a: Option[FindAccountsResp], req: Authenticate): Receive = {
    case m: FindRealmsResponse if r.isEmpty && a.isEmpty =>
      context.become(waiting(client, Some(m), a, req))
    case m: FindAccountsResp if r.isEmpty && a.isEmpty =>
      context.become(waiting(client, r, Some(m), req))

    case m: FindRealmsResponse if a.nonEmpty =>
      createToken(client, m.realms.headOption, a.get.accounts.headOption, req)

    case m: FindAccountsResp if r.nonEmpty =>
      createToken(client, r.get.realms.headOption, m.accounts.headOption, req)
  }

  private def createToken(client: ActorRef, realm: Option[Realm], acc: Option[Account], req: Authenticate) {
    (realm, acc) match {
      case (Some(r), Some(a)) =>
        val token = AuthToken(r, a, req.creds)
        handlers.foreach(_ ! token)
        context.become(authenticating(client, token, req, handlers.toSet))
      case _ =>
        client ! AuthenticateResp(None, req.id)
        context.stop(self)
    }
  }

  def authenticating(client: ActorRef, token: AuthToken, req: Authenticate, refs: Set[ActorRef]): Receive = {
    case t:AuthToken if refs.size == 1 =>
      val merged = merge(token, t)
      client ! AuthenticateResp(Some(merged.toResult), req.id)
      context.stop(self)

    case t:AuthToken if refs.size > 1 =>
      context.become(authenticating(client, merge(token, t), req, refs - sender))
  }
}

object AuthcWorker {
  import porter.model._

  def handlerProps(auth: Authenticator) = Props(classOf[HandlerActor], auth)
  def workerProps(store: ActorRef, handlers: Iterable[Authenticator]) =
    Props(classOf[AuthcWorker], store, handlers)

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

  case class Authenticate(realmId: Ident, creds: Set[Credentials], id: Int = 0) extends RealmMessage
  case class AuthenticateResp(result: Option[AuthResult], id: Int) extends PorterMessage
}

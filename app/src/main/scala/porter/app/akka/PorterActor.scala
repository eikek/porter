package porter.app.akka

import akka.actor.Actor
import porter.auth.Porter
import akka.actor.Status.Failure
import porter.store.MutableStore
import scala.concurrent.Future
import porter.model.Ident


/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 16:23
 */
class PorterActor(porter: Porter) extends Actor {
  import PorterActor._
  import context.dispatcher
  import akka.pattern.pipe

  def receive = {
    case req @ GetPolicy(realm, acc) =>
      porter.getPolicy(realm, acc)
        .map(PolicyResponse(req, _)).pipeTo(sender)

    case Authenticate(realm, creds) =>
      porter.authenticate(realm, creds)
        .map(AuthResponse.apply).pipeTo(sender)

    case req @ Authorized(realm, acc, perms) =>
      porter.authorized(realm, acc, perms)
        .map(AuthzResponse(req, _)).pipeTo(sender)

    case req @ FindAccount(realm, acc) =>
      porter.store.findAccounts(realm, acc)
        .map(AccountResponse(req, _)).pipeTo(sender)

    case req @ FindGroup(realm, gids) =>
      porter.store.findGroups(realm, gids)
        .map(GroupResponse(req, _)).pipeTo(sender)

    case FindRealm(ids) =>
      porter.store.findRealms(ids).pipeTo(sender)

    case ListRealms =>
      porter.store.allRealms.pipeTo(sender)

    case ListAccounts(realm) =>
      porter.store.allAccounts(realm).pipeTo(sender)

    case ListGroups(realm) =>
      porter.store.allGroups(realm).pipeTo(sender)

    case UpdateRealm(realm) =>
      withMutableStore(realm.id)(s => s.updateRealm(realm))

    case UpdateAccount(realm, acc) =>
      withMutableStore(realm)(s => s.updateAccount(realm, acc))

    case UpdateGroup(realm, group) =>
      withMutableStore(realm)(s => s.updateGroup(realm, group))

    case DeleteRealm(realm) =>
      withMutableStore(realm)(s => s.deleteRealm(realm))

    case DeleteAccount(realm, acc) =>
      withMutableStore(realm)(s => s.deleteAccount(realm, acc))

    case DeleteGroup(realm, group) =>
      withMutableStore(realm)(s => s.deleteGroup(realm, group))
  }

  private def withMutableStore[A](realm: Ident)(f: MutableStore => Future[A]) {
    porter.mutableStore(realm) match {
      case Some(s) => f(s).pipeTo(sender)
      case _ => sender ! Failure(new IllegalStateException("No mutable store available"))
    }
  }
}

object PorterActor {
  import porter.model._
  import porter.auth._

  sealed trait PorterMessage extends Serializable

  case class GetPolicy(realm: Ident, acc: Ident) extends PorterMessage
  case class PolicyResponse(req: GetPolicy, policy: Policy) extends PorterMessage

  case class Authenticate(realm: Ident, creds: Set[Credentials]) extends PorterMessage
  case class AuthResponse(token: AuthResult) extends PorterMessage

  case class Authorized(realm: Ident, account: Ident, perms: Set[Permission]) extends PorterMessage
  case class AuthzResponse(req: Authorized, result: Boolean) extends PorterMessage

  case class FindAccount(realm: Ident, accounts: Set[Ident]) extends PorterMessage
  case class AccountResponse(req: FindAccount, accounts: Iterable[Account]) extends PorterMessage

  case class FindGroup(realm: Ident, groups: Set[Ident]) extends PorterMessage
  case class GroupResponse(req: FindGroup, groups: Iterable[Group]) extends PorterMessage

  case class FindRealm(realms: Set[Ident]) extends PorterMessage

  case object ListRealms extends PorterMessage
  case class ListAccounts(realm: Ident) extends PorterMessage
  case class ListGroups(realm: Ident) extends PorterMessage

  case class UpdateRealm(realm: Realm) extends PorterMessage
  case class UpdateAccount(realm: Ident, acc: Account) extends PorterMessage
  case class UpdateGroup(realm: Ident, acc: Group) extends PorterMessage

  case class DeleteRealm(realm: Ident) extends PorterMessage
  case class DeleteAccount(realm: Ident, acc: Ident) extends PorterMessage
  case class DeleteGroup(realm: Ident, group: Ident) extends PorterMessage
}
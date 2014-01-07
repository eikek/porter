package porter.client

import porter.model._
import porter.auth.AuthResult

object Messages {
  //nesting message objects in other objects yields compiler warnings like:
  //   "The outer reference in this type test cannot be checked at run time."
  //This means, that it is not possible to determine from which outer reference
  //the nested type comes from. This is no problem, since we use only type
  //aliases and I still like to group those messages...
  // see http://stackoverflow.com/questions/16450008/typesafe-swing-eventsthe-outer-reference-in-this-type-test-cannot-be-checked-a
  // and https://issues.scala-lang.org/browse/SI-4440

  trait PorterMessage extends Serializable

  object store {
    trait StoreMessage extends PorterMessage

    case class FindAccounts(realm: Ident, names: Set[Ident]) extends StoreMessage
    case class FindAccountsResp(accounts: Set[Account]) extends StoreMessage

    case class FindGroups(realm: Ident, names: Set[Ident]) extends StoreMessage
    case class FindGroupsResp(groups: Set[Group]) extends StoreMessage

    case class FindRealms(names: Set[Ident]) extends StoreMessage
    case class FindRealmsResp(realms: Set[Realm]) extends StoreMessage
  }

  object mutableStore {
    trait MutableStoreMessage extends PorterMessage {
      def realmId: Ident
    }

    case class UpdateRealm(realm: Realm) extends MutableStoreMessage {
      val realmId = realm.id
    }
    case class DeleteRealm(realmId: Ident) extends MutableStoreMessage
    case class UpdateAccount(realmId: Ident, account: Account) extends MutableStoreMessage
    case class DeleteAccount(realmId: Ident, account: Ident) extends MutableStoreMessage
    case class UpdateGroup(realmId: Ident, group: Group) extends MutableStoreMessage
    case class DeleteGroup(realmId: Ident, group: Ident) extends MutableStoreMessage
    case class OperationFinished(result: Boolean) extends PorterMessage
    case class ChangePassword(realm: Ident, current: Credentials, plain: String)
  }

  object auth {
    case class Authenticate(realmId: Ident, creds: Set[Credentials]) extends PorterMessage
    case class AuthenticateResp(result: Option[AuthResult]) extends PorterMessage

    case class AuthAccount(success: Boolean, account: Option[Account])

    case class Authorize(realm: Ident, account: Ident, perms: Iterable[String]) extends PorterMessage
    case class AuthorizeResp(realm: Ident, account: Ident, authorized: Boolean) extends PorterMessage
  }
}

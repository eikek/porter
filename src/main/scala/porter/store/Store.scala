package porter.store

import scala.concurrent.Future
import porter.model.Ident

/**
 *
 * @since 22.11.13 19:29
 *
 */
trait Store {

  import porter.model._

  def findRealms(names: Set[Ident]): Future[Iterable[Realm]]

  def findAccounts(realm: Ident, names: Set[Ident]): Future[Iterable[Account]]

  def findAccountsFor(realm: Ident, creds: Set[Credentials]): Future[Iterable[Account]]

  def findGroups(realm: Ident, names: Set[Ident]): Future[Iterable[Group]]

  def allRealms(): Future[Iterable[Realm]]

  def allAccounts(realm: Ident): Future[Iterable[Account]]

  def allGroups(realm: Ident): Future[Iterable[Group]]
}

trait MutableStore {
  import porter.model._

  def updateRealm(realm: Realm): Future[Boolean]

  def deleteRealm(realm: Ident): Future[Boolean]

  def updateAccount(realm: Ident, account: Account): Future[Boolean]

  def deleteAccount(realm: Ident, accId: Ident): Future[Account]

  def updateGroup(realm: Ident, group: Group): Future[Boolean]

  def deleteGroup(realm: Ident, groupId: Ident): Future[Group]
}

trait StoreProvider {

  def store: Store

  def mutableStore: Option[MutableStore] = None
}
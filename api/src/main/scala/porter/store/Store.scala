package porter.store

import scala.concurrent.{ExecutionContext, Future}
import porter.model.Ident

/**
 *
 * @since 22.11.13 19:29
 *
 */
trait Store {

  import porter.model._

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext): Future[Iterable[Realm]]

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext): Future[Iterable[Account]]

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext): Future[Iterable[Account]]

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext): Future[Iterable[Group]]

  def allRealms(implicit ec: ExecutionContext): Future[Iterable[Realm]]

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext): Future[Iterable[Account]]

  def allGroups(realm: Ident)(implicit ec: ExecutionContext): Future[Iterable[Group]]
}

trait MutableStore {
  import porter.model._

  def updateRealm(realm: Realm)(implicit ec: ExecutionContext): Future[Boolean]

  def deleteRealm(realm: Ident)(implicit ec: ExecutionContext): Future[Boolean]

  def updateAccount(realm: Ident, account: Account)(implicit ec: ExecutionContext): Future[Boolean]

  def deleteAccount(realm: Ident, accId: Ident)(implicit ec: ExecutionContext): Future[Boolean]

  def updateGroup(realm: Ident, group: Group)(implicit ec: ExecutionContext): Future[Boolean]

  def deleteGroup(realm: Ident, groupId: Ident)(implicit ec: ExecutionContext): Future[Boolean]
}

trait StoreProvider {

  def store: Store

  def mutableStore(realm: Ident): Option[MutableStore] = None
}
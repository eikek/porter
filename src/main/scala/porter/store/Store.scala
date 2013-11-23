package porter.store

import scala.util.Try
import scala.concurrent.Future

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

trait StoreProvider {

  def store: Store

}
package porter.store

import scala.util.Try

/**
 *
 * @since 22.11.13 19:29
 *
 */
trait Store {

  import porter.model._

  def findRealms(names: Set[Ident]): Try[Iterable[Realm]]

  def findAccounts(realm: Ident, names: Set[Ident]): Try[Iterable[Account]]

  def findAccountsFor(realm: Ident, creds: Set[Credentials]): Try[Iterable[Account]]

  def findGroups(realm: Ident, names: Set[Ident]): Try[Iterable[Group]]

  def allRealms(): Try[Iterable[Realm]]

  def allAccounts(realm: Ident): Try[Iterable[Account]]

  def allGroups(realm: Ident): Try[Iterable[Group]]
}

trait StoreProvider {

  def store: Store

}
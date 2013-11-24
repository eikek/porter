package porter.store

import porter.model.{Credentials, Ident}
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 18:33
 */
trait MultiStore extends Store {

  def stores: Iterable[Store]

  implicit def executionContext: ExecutionContext

  def findRealms(names: Set[Ident]) =
    flatten(s => s.findRealms(names))

  def findAccounts(realm: Ident, names: Set[Ident]) =
    flatten(s => s.findAccounts(realm, names))

  def findAccountsFor(realm: Ident, creds: Set[Credentials]) =
    flatten(s => s.findAccountsFor(realm, creds))

  def findGroups(realm: Ident, names: Set[Ident]) =
    flatten(s => s.findGroups(realm, names))

  def allRealms() = flatten(s => s.allRealms())

  def allAccounts(realm: Ident) = flatten(s => s.allAccounts(realm))

  def allGroups(realm: Ident) = flatten(s => s.allGroups(realm))

  private def flatten[A](f: Store => Future[Iterable[A]]) =
    Future.sequence(stores map f).map(_.flatten)

}

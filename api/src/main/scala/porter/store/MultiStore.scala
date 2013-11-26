package porter.store

import porter.model.{Credentials, Ident}
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 18:33
 */
trait MultiStore extends Store {

  def stores: Iterable[Store]

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) =
    flatten(s => s.findRealms(names))

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) =
    flatten(s => s.findAccounts(realm, names))

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) =
    flatten(s => s.findAccountsFor(realm, creds))

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) =
    flatten(s => s.findGroups(realm, names))

  def allRealms(implicit ec: ExecutionContext) =
    flatten(s => s.allRealms)

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) =
    flatten(s => s.allAccounts(realm))

  def allGroups(realm: Ident)(implicit ec: ExecutionContext) =
    flatten(s => s.allGroups(realm))

  private def flatten[A](f: Store => Future[Iterable[A]])(implicit ec: ExecutionContext) =
    Future.sequence(stores map f).map(_.flatten)

}

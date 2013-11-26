package porter.store


import scala.util.{Success, Try}
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 14:15
 */
trait SimpleStore extends Store {
  import porter.model._

  def realms: Iterable[Realm]

  def groups: Iterable[(Realm, Group)]

  def accounts: Iterable[(Realm, Account)]

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) = Future.successful {
    realms filter (r => names.contains(r.id))
  }

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = Future.successful {
    for {
      (r, a) <- accounts
      if r.id == realm && names.contains(a.name)
    } yield a
  }

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) = {
    lazy val nameSet = creds.collect({ case ac: AccountCredentials => ac.accountName })
    findAccounts(realm, nameSet)
  }

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = Future.successful {
    for {
      (r, g) <- groups
      if r.id == realm && names.contains(g.name)
    } yield g
  }

  def allRealms(implicit ec: ExecutionContext) = Future.successful(realms)

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) =
    Future.successful(for ((r,a) <- accounts; if r.id == realm) yield a)

  def allGroups(realm: Ident)(implicit ec: ExecutionContext) =
    Future.successful(for ((r,g) <- groups; if r.id == realm) yield g)
}

package porter.store


import scala.util.{Success, Try}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 14:15
 */
trait SimpleStore extends Store {
  import porter.model._

  def realms: Iterable[Realm]

  def groups: Iterable[(Realm, Group)]

  def accounts: Iterable[(Realm, Account)]

  def findRealms(names: Set[Ident]) = Try {
    realms filter (r => names.contains(r.id))
  }

  def findAccounts(realm: Ident, names: Set[Ident]) = Try {
    for {
      (r, a) <- accounts
      if r.id == realm && names.contains(a.name)
    } yield a
  }

  def findAccountsFor(realm: Ident, creds: Set[Credentials]) = {
    lazy val nameSet = creds.collect({ case ac: AccountCredentials => ac.accountName })
    findAccounts(realm, nameSet)
  }

  def findGroups(realm: Ident, names: Set[Ident]) = Try {
    for {
      (r, g) <- groups
      if r.id == realm && names.contains(g.name)
    } yield g
  }

  def allRealms() = Success(realms)

  def allAccounts(realm: Ident) = Try(for ((r,a) <- accounts; if r.id == realm) yield a)

  def allGroups(realm: Ident) = Try(for ((r,g) <- groups; if r.id == realm) yield g)
}

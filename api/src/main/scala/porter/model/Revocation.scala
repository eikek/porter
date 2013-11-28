package porter.model

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 23:06
 */
final case class Revocation(perm: Permission) {
  def revokes(p: Permission) = perm implies p
  override def toString = "!"+perm.toString
}

object Revocation {

  def revokesAll(given: Iterable[Revocation], check: Iterable[Permission]): Boolean =
    Permission.impliesAll(given.map(_.perm), check)

  def revokesOne(given: Iterable[Revocation], check: Iterable[Permission]): Boolean =
    Permission.impliesOne(given.map(_.perm), check)

  def reduce(rev: Iterable[Revocation]): Set[Revocation] = {
    val ps = Permission.reduce(rev.map(_.perm))
    ps.map(Revocation.apply)
  }

  def union(one: Set[Revocation], two: Set[Revocation]): Set[Revocation] =
    Permission.union(one.map(_.perm), two.map(_.perm)).map(apply)

  def diff(current: Set[Revocation], remove: Set[Revocation]): Set[Revocation] =
    Permission.diff(current.map(_.perm), remove.map(_.perm)).map(apply)
}

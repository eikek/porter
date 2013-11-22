package porter.model

import scala.annotation.tailrec

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 20:29
 */
trait Permission extends Serializable {

  def implies(other: Permission): Boolean

}

object Permission {

  val allPermission = new Permission {
    def implies(other: Permission) = true
    override def toString = "AllPermission"
  }

  def apply(str: String) = DefaultPermission(str)

  /**
   * Returns true if all permissions is "check" are covered by the permissions
   * in "given".
   *
   *@param given
   * @param check
   * @return
   */
  def impliesAll(given: Iterable[Permission], check: Iterable[Permission]): Boolean =
    check.forall(pb => given.exists(pa => pa implies pb))

  /**
   * Returns true if at least one of the permissions in "check" is covered
   * by the permissions in "given".
   *
   * @param given
   * @param check
   * @return
   */
  def impliesOne(given: Iterable[Permission], check: Iterable[Permission]): Boolean =
    check.exists(pb => given.exists(pa => pa implies pb))

  def reduce(perms: Iterable[Permission]): Set[Permission] = {
    @tailrec
    def loop(ps: Iterable[Permission], acc: Set[Permission]): Set[Permission] = {
      if (ps.isEmpty) acc
      else {
        val next = ps.take(1).toList
        val rest = ps.drop(1)
        if (impliesAll(rest, next)) loop(rest, acc)
        else loop(rest, acc ++ next)
      }
    }
    loop(perms.toList, Set.empty)
  }

}
package porter.model

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

  /**
   * Reduces the list of permissions by removing those that are
   * implied by others in the list.
   *
   * @param perms
   * @return
   */
  def reduce(perms: Iterable[Permission]): Set[Permission] = {
    perms.foldLeft(perms.toSet) { (set, p) =>
      val rest = set.filterNot(_ == p)
      if (impliesAll(rest, List(p))) rest else set
    }
  }
}
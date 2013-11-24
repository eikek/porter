package porter.model

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 23:05
 */
final class Policy(_rules: Set[Rule]) {

  lazy val (permissions, revocations) = partitionRules(_rules) match {
    case (p, r) => (Permission.reduce(p), Permission.reduce(r.map(_.perm)))
  }

  lazy val rules = permissions.map(permRule) ++
    revocations.map(Revocation.apply).map(revocRule)

  def grantsAll(perms: Iterable[Permission]): Boolean = {
    val allowed = Permission.impliesAll(permissions, perms)
    val revoked = Permission.impliesOne(revocations, perms)
    allowed && !revoked
  }
}
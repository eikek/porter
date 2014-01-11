package porter.model

final class Policy(_rules: Set[Rule]) {

  lazy val (permissions, revocations) = Rules.partition(Rules.reduce(_rules)) match {
    case (p, r) => (p, r.map(_.perm))
  }

  lazy val rules = permissions.map(permRule) ++
    revocations.map(Revocation.apply).map(revocRule)

  def grantsAll(perms: Iterable[Permission]): Boolean = {
    val allowed = Permission.impliesAll(permissions, perms)
    val revoked = Permission.impliesOne(revocations, perms)
    allowed && !revoked
  }

  override def equals(a: Any): Boolean = a match {
    case p: Policy => p.rules == this.rules
    case _ => false
  }

  override lazy val hashCode: Int = rules.hashCode()

  override lazy val toString =
    if (rules.isEmpty) "Policy()"
    else s"Policy(${permissions.map(_.toString).mkString(",")}, ${revocations.map(r => "!"+r.toString).mkString(",")})"
}

object Policy {

  val empty = apply(Set())

  def apply(rules: Set[Rule]): Policy = new Policy(rules)

  def unapply(p: Policy): Option[(Set[Permission], Set[Revocation])] = Rules.unapply(p.rules)

}
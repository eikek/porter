package porter

package object model {
  import scala.language.implicitConversions

  type Properties = Map[String, String]

  type Parts = List[Set[String]]

  type Rule = Either[Revocation, Permission]

  type PermissionFactory = PartialFunction[String, Permission]

  implicit def permRule(p:Permission): Rule = Right(p)
  implicit def revocRule(r:Revocation): Rule = Left(r)

  object Rules {

    def partition(rules: Iterable[Rule]): (Set[Permission], Set[Revocation]) = {
      val (perm, revocs) = rules.partition(_.isRight)
      (perm.map(_.right.get).toSet, revocs.map(_.left.get).toSet)
    }

    def unapply(rules: Iterable[Rule]): Option[(Set[Permission], Set[Revocation])] =
      Some(partition(rules))

    def reduce(rules: Set[Rule]): Set[Rule] = {
      //remove all permissions, that are implied by others in the list
      val (perm, rev) = partition(rules)  match {
        case (p, r) => (Permission.reduce(p), Permission.reduce(r.map(_.perm)))
      }
      //remove permission that are revoked
      val ps = Permission.diff(perm, rev)
      //remove revocations, which are not covered by any partition
      val rv = rev.filter(rp => Permission.impliesOne(ps, Set(rp)))
      ps.map(permRule) ++ rv.map(Revocation.apply).map(revocRule)
    }
  }
}
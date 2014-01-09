package porter

package object model {
  import scala.language.implicitConversions

  type Properties = Map[String, String]

  type Parts = List[Set[String]]

  type Rule = Either[Revocation, Permission]

  type PermissionFactory = PartialFunction[String, Permission]

  implicit def permRule(p:Permission): Rule = Right(p)
  implicit def revocRule(r:Revocation): Rule = Left(r)

  def partitionRules(rules: Iterable[Rule]): (Set[Permission], Set[Revocation]) = {
    val (perm, revocs) = rules.partition(_.isRight)
    (perm.map(_.right.get).toSet, revocs.map(_.left.get).toSet)
  }
}
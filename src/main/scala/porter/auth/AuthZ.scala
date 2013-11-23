package porter.auth

import porter.store.StoreProvider
import scala.util.Try

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 23:50
 */
trait AuthZ {
  self: StoreProvider with RuleProvider =>

  import porter.model._

  def getPolicy(realm: Ident)(account: Ident): Try[Policy] = {
    val rules = for {
      a <- store.findAccounts(realm, Set(account))
      if a.nonEmpty
      groups <- store.findGroups(realm, a.toList(0).groups)
    } yield groups.flatMap(_.rules)
    Try {
      val toRule = createRuleWith(permissionFactory)_
      new Policy(rules.get.map(rs => toRule(rs).get).toSet)
    }
  }

  def authorized(realm: Ident)(account: Ident, perms: Iterable[Permission]): Boolean =
    getPolicy(realm)(account).map(_ grantsAll perms).getOrElse(false)
}

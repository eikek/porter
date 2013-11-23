package porter.auth

import porter.store.StoreProvider
import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 23:50
 */
trait AuthZ {
  self: StoreProvider with RuleFactory =>

  import porter.model._

  def getPolicy(realm: Ident, account: Ident)(implicit ec: ExecutionContext): Future[Policy] = {
    val rules = for {
      a <- store.findAccounts(realm, Set(account))
      if a.nonEmpty
      groups <- store.findGroups(realm, a.toList(0).groups)
    } yield groups.flatMap(_.rules)
    rules map { rstr =>
      val toRule = createRuleWith(permissionFactory)_
      new Policy(rstr.map(s => toRule(s).get).toSet)
    }
  }

  def authorized(realm: Ident, account: Ident, perms: Iterable[Permission])(implicit ec: ExecutionContext): Future[Boolean] =
    getPolicy(realm, account).map(_ grantsAll perms)
}

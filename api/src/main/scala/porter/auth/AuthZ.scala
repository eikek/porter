/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porter.auth

import porter.store.StoreProvider
import scala.util.{Failure, Success, Try}
import scala.concurrent.{ExecutionContext, Future}

trait AuthZ {
  self: StoreProvider with RuleFactory =>

  import porter.model._

  final def getPolicy(realm: Ident, account: Ident)(implicit ec: ExecutionContext): Future[Policy] = {
    val rules = for {
      a <- store.findAccounts(realm, Set(account))
      if a.nonEmpty
      groups <- store.findGroups(realm, a.toList(0).groups)
    } yield groups.flatMap(_.rules)
    val f = rules.transform(identity, ex =>
      new IllegalStateException("Error getting policy!", ex))
    f map { rstr =>
      val toRule = RuleFactory.createRuleWith(permissionFactory)_
      new Policy(rstr.map(s => toRule(s).get).toSet)
    }
  }

  final def authorized(realm: Ident, account: Ident, perms: Iterable[Permission])(implicit ec: ExecutionContext): Future[Boolean] =
    getPolicy(realm, account).map(_ grantsAll perms)

  final def authorizedPlain(realm: Ident, account: Ident, perms: Iterable[String])
                     (implicit ec: ExecutionContext): Future[Boolean] =
    Try(perms.map(permissionFactory)) match {
      case Success(ps) => authorized(realm, account, ps)
      case Failure(ex) => Future.failed(new IllegalArgumentException("Cannot create permissions!", ex))
    }
}

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

package porter.store


import scala.concurrent.{ExecutionContext, Future}

trait SimpleStore extends Store {
  import porter.model._
  import porter.util._

  def realms: Iterable[Realm]

  def groups: Iterable[(Realm, Group)]

  def accounts: Iterable[(Realm, Account)]

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) = Future.successful {
    realms filter (r => names.contains(r.id))
  }

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = Future.immediate {
    for {
      (r, a) <- accounts
      if r.id == realm && names.contains(a.name)
    } yield a
  }

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) = {
    lazy val nameSet = creds.collect({ case ac: AccountCredentials => ac.accountName })
    findAccounts(realm, nameSet)
  }

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = Future.immediate {
    for {
      (r, g) <- groups
      if r.id == realm && names.contains(g.name)
    } yield g
  }

  def allRealms(implicit ec: ExecutionContext) = Future.immediate(realms)

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) =
    Future.immediate(for ((r,a) <- accounts; if r.id == realm) yield a)

  def allGroups(realm: Ident)(implicit ec: ExecutionContext) =
    Future.immediate(for ((r,g) <- groups; if r.id == realm) yield g)

  def close() {}
}

object SimpleStore {
  import porter.model._
  import porter.util._

  def apply(r: Iterable[Realm], g: Iterable[(Realm, Group)], a: Iterable[(Realm, Account)]): SimpleStore =
    new SimpleStore {
      val accounts = a
      val realms = r
      val groups = g
    }
}
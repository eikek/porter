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

import porter.model.{Credentials, Ident}
import scala.concurrent.{ExecutionContext, Future}

trait MultiStore extends Store {

  def stores: Iterable[Store]

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) =
    flatten(s => s.findRealms(names))

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) =
    flatten(s => s.findAccounts(realm, names))

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) =
    flatten(s => s.findAccountsFor(realm, creds))

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) =
    flatten(s => s.findGroups(realm, names))

  def allRealms(implicit ec: ExecutionContext) =
    flatten(s => s.allRealms)

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) =
    flatten(s => s.allAccounts(realm))

  def allGroups(realm: Ident)(implicit ec: ExecutionContext) =
    flatten(s => s.allGroups(realm))

  private def flatten[A](f: Store => Future[Iterable[A]])(implicit ec: ExecutionContext) =
    Future.sequence(stores map f).map(_.flatten)

}

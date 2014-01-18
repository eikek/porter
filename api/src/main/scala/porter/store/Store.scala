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
import porter.model.Ident

trait Store {

  import porter.model._

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext): Future[Iterable[Realm]]

  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext): Future[Iterable[Account]]

  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext): Future[Iterable[Account]]

  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext): Future[Iterable[Group]]

  def allRealms(implicit ec: ExecutionContext): Future[Iterable[Realm]]

  def allAccounts(realm: Ident)(implicit ec: ExecutionContext): Future[Iterable[Account]]

  def allGroups(realm: Ident)(implicit ec: ExecutionContext): Future[Iterable[Group]]

  def close()
}

trait MutableStore {
  import porter.model._

  def updateRealm(realm: Realm)(implicit ec: ExecutionContext): Future[Boolean]

  def deleteRealm(realm: Ident)(implicit ec: ExecutionContext): Future[Boolean]

  def updateAccount(realm: Ident, account: Account)(implicit ec: ExecutionContext): Future[Boolean]

  def deleteAccount(realm: Ident, accId: Ident)(implicit ec: ExecutionContext): Future[Boolean]

  def updateGroup(realm: Ident, group: Group)(implicit ec: ExecutionContext): Future[Boolean]

  def deleteGroup(realm: Ident, groupId: Ident)(implicit ec: ExecutionContext): Future[Boolean]

  def close()
}

trait StoreProvider {

  def store: Store

  def mutableStore(realm: Ident): Option[MutableStore] = None
}
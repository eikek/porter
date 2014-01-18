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

package porter.app.akka.api

import porter.store.{MutableStore, SimpleStore}
import scala.concurrent.{Future, ExecutionContext}

object TestData {
  import porter.model._

  val testpassword = Password("test")

  val john = Account(
    name = "john",
    groups = Set("g1", "g2"),
    secrets = Seq(testpassword)
  )
  val policyJohn = Policy(Set(
    DefaultPermission("perm1:x"),
    DefaultPermission("perm1:y:1"),
    ResourcePermission("resource:read:/**")))

  val mary = Account(
    name = "mary",
    groups = Set("g2", "g4"),
    secrets = Seq(testpassword)
  )

  val g1 = Group(name = "g1", rules = Set("perm1:x:1", "perm1:y:1", "resource:read:/main/**"))
  val g2 = Group(name = "g2", rules = Set("perm1:x", "resource:read:/**"))
  val g3 = Group(name = "g3", rules = Set("perm1:y", "resource:read:/**"))
  val g4 = Group(name = "g4", rules = Set("perm1", "resource:read:/**"))

  val realm = Realm("r1", "Test Realm")

  val store1 = createStore(
    realms = List(realm),
    groups = List(realm -> g1, realm -> g2),
    accounts = List(realm -> john, realm -> mary)
  )
  val store2 = createStore(
    realms = List(realm, Realm("r4", "")),
    groups = List(realm -> g3, realm -> g4),
    accounts = List(realm -> Account("james"), realm -> Account("gloria"))
  )
  
  def createStore(realms: Iterable[Realm] = Set(),
                  accounts: Iterable[(Realm, Account)] = Set(),
                  groups: Iterable[(Realm, Group)] = Set()) = SimpleStore(realms, groups, accounts)


  class EmptyMutableStore extends MutableStore {
    def updateRealm(realm: Realm)(implicit ec: ExecutionContext) = Future.successful(true)
    def deleteRealm(realm: Ident)(implicit ec: ExecutionContext) = Future.successful(true)
    def updateAccount(realm: Ident, account: Account)(implicit ec: ExecutionContext) = Future.successful(true)
    def deleteAccount(realm: Ident, accId: Ident)(implicit ec: ExecutionContext) = Future.successful(true)
    def updateGroup(realm: Ident, group: Group)(implicit ec: ExecutionContext) = Future.successful(true)
    def deleteGroup(realm: Ident, groupId: Ident)(implicit ec: ExecutionContext) = Future.successful(true)
    def close() = {}
  }
}

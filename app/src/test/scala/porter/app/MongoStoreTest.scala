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

package porter.app

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import com.typesafe.config.ConfigFactory
import scala.concurrent.Await
import porter.auth.{AuthToken, PasswordValidator}

class MongoStoreTest extends FunSuite with ShouldMatchers with BeforeAndAfterAll {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.language.postfixOps
  import scala.concurrent.duration._
  import porter.model._

  //note, these tests only work with a running mongodb instance

  val config = ConfigFactory.load().getConfig("porter-mongo-test")

  val store = new MongoStore(config)


  override protected def afterAll() = {
    val client = MongoStore.createClient(config)
    client(config.getString("dbname")).dropDatabase()
  }

  private def createRealm(): Realm = {
    val r = Realm(Ident.randomIdent, "Some Realm")
    Await.ready(store.updateRealm(r), 5 seconds)
    r
  }

  test("create and list realms") {
    val r = createRealm()
    val all = Await.result(store.allRealms, 5 seconds)
    all should contain (r)
  }

  test("create and list groups") {
    val r = createRealm()
    val g1 = Group("g1", Map("enabled" -> "true"), Set("some:perm:1"))
    val g2 = Group("g2", Map.empty, Set("some:perm:2", "!some:perm:3"))
    Await.result(store.updateGroup(r.id, g1), 5 seconds)
    Await.result(store.updateGroup(r.id, g2), 5 seconds)
    val all = Await.result(store.allGroups(r.id), 5 seconds).toSet
    all should contain (g1)
    all should contain (g2)

    val og1 = Await.result(store.findGroups(r.id, Set("g1")), 5 seconds)
    og1 should be (List(g1))

    Await.ready(store.updateGroup(r.id, g2.updatedRules(r => Set.empty)), 5 seconds)
    val og2 = Await.result(store.findGroups(r.id, Set("g2")), 5 seconds)
    og2 should not be List(g2)
    og2(0).name should be (g2.name)
    og2(0).props should be (g2.props)

    Await.ready(store.deleteGroup(r.id, g2.name), 5 seconds)
    val empty = Await.result(store.findGroups(r.id, Set(g2.name)), 5 seconds)
    empty should be (List())
  }

  test("create and list accounts") {
    val r = createRealm()
    val passw = Password("test")
    val acc1 = Account("john", Map("enabled" -> "true"), Set("g1", "g3"), Seq(passw))
    val acc2 = Account("mary", Map("enabled" -> "false"), Set("g1", "g2"), Seq(passw))

    Await.ready(store.updateAccount(r.id, acc1), 5 seconds)
    Await.ready(store.updateAccount(r.id, acc2), 5 seconds)

    val all = Await.result(store.allAccounts(r.id), 5 seconds)
    all.toSet should be (Set(acc1, acc2))

    val l1 = Await.result(store.findAccounts(r.id, Set(acc1.name)), 5 seconds)
    l1 should be (List(acc1))

    val cl1 = Await.result(store.findAccountsFor(r.id, Set(PasswordCredentials("john", "test"))), 5 seconds)
    cl1 should be (l1)

    Await.ready(store.deleteAccount(r.id, acc1.name), 5 seconds)
    val l2 = Await.result(store.allAccounts(r.id), 5 seconds)
    l2 should be (List(acc2))

    Await.ready(store.updateAccount(r.id, acc2.updatedGroups(s => Set.empty)), 5 seconds)
    val l2u = Await.result(store.findAccounts(r.id, Set(acc2.name)), 5 seconds)
    l2u should not be List(acc2)
    l2u(0).groups should have size 0
    val to = PasswordValidator.authenticate(AuthToken(r, acc2, Set(PasswordCredentials("mary", "test"))))
    to.toResult.successCount should be (1)
  }

  test("find groups") {
    val r = createRealm()
    val g1 = Group("g1", Map("enabled" -> "true"), Set("some:perm:1"))
    val g2 = Group("g2", Map.empty, Set("some:perm:2", "!some:perm:3"))
    Await.result(store.updateGroup(r.id, g1), 5 seconds)
    Await.result(store.updateGroup(r.id, g2), 5 seconds)
    val list = Await.result(store.findGroups(r.id, Set("g1", "g2")), 5 seconds)
    list.toSet should be (Set(g1, g2))
  }
}

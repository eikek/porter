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

import _root_.akka.actor.ActorSystem
import porter.auth.PasswordValidator
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import com.typesafe.config.ConfigFactory
import porter.store.{Store, MutableStore}
import porter.model._
import porter.model.Group
import porter.model.Realm
import porter.model.Account
import scala.concurrent.ExecutionContext

class PorterSettingsTest extends FunSuite with ShouldMatchers with BeforeAndAfterAll {

  val system = ActorSystem("PorterSettingsTest")
  override  def afterAll() {
    system.shutdown()
  }

  test("read config") {
    val cfg = ConfigFactory.parseString(
      """
        |validators: [ { class: "porter.auth.PasswordValidator", params: {} } ]
        |stores: [
        |  { class: "porter.app.ConfigStore", params: ${storeconfig}, realms: [] },
        |  { class: "porter.app.TestMStore", params: {}, realms: [ "realm1" ] }
        |]
        |permissionFactories: [
        |  { class: "porter.app.TestFactory", params: {} }
        |]
        |storeconfig: {
        |
        |}
      """.stripMargin)

    val settings = PorterSettings.fromConfig(system, cfg.resolve())
    settings.validators should have size 1
    settings.validators(0) should be (PasswordValidator)
    settings.stores should have size 2
    settings.stores(0).getClass should be (classOf[ConfigStore])
    settings.mutableStores should have size 1
    settings.mutableStores(0)._1 should be (Set(Ident("realm1")))

    settings.permissionFactories should have size 1
    settings.permissionFactories(0).getClass should be (classOf[TestFactory])
  }
}

class TestFactory extends PermissionFactory {
  def apply(v1: String) = Permission.allPermission
  def isDefinedAt(x: String) = false
}
class TestMStore extends Store with MutableStore {
  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) = ???
  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = ???
  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) = ???
  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = ???
  def allRealms(implicit ec: ExecutionContext) = ???
  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) = ???
  def allGroups(realm: Ident)(implicit ec: ExecutionContext) = ???
  def updateRealm(realm: Realm)(implicit ec: ExecutionContext) = ???
  def deleteRealm(realm: Ident)(implicit ec: ExecutionContext) = ???
  def updateAccount(realm: Ident, account: Account)(implicit ec: ExecutionContext) = ???
  def deleteAccount(realm: Ident, accId: Ident)(implicit ec: ExecutionContext) = ???
  def updateGroup(realm: Ident, group: Group)(implicit ec: ExecutionContext) = ???
  def deleteGroup(realm: Ident, groupId: Ident)(implicit ec: ExecutionContext) = ???
  def close() = ???
}

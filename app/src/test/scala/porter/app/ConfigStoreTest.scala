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

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.model._
import com.typesafe.config.ConfigFactory
import porter.model.Realm

class ConfigStoreTest extends FunSuite with ShouldMatchers {
  val readOnly = PropertyList.mutableSource.toFalse
  val testpw = Password("test")

  test("read one realm with one account and two groups") {
    val cstr =
      s"""
        |realm1: {
        |   name: "my great realm"
        |   groups: {
        |      admin: {
        |        rules: [ "bla.bla", "!bla.bla.blop" ]
        |        props: {
        |          key: "value"
        |        }
        |      },
        |      users: {
        |        rules: [ "resource:read:/**" ]
        |      }
        |    }
        |    accounts: {
        |      john: {
        |        secret: "${testpw.asString}"
        |        groups: [ "admin", "users" ]
        |        props: {
        |          enabled: "true"
        |        }
        |      }
        |    }
        | }
      """.stripMargin
    val cfg = ConfigFactory.parseString(cstr)
    val store = new ConfigStore(cfg)
    store.realms should have size 1
    store.realms(0) should be (Realm("realm1", "my great realm"))
    store.groups should have size 2
    val groups = store.groups.map({ case(r,g) => g})
    groups(0).name should (be (Ident("admin")) or be (Ident("users")))
    groups(1).name should (be (Ident("admin")) or be (Ident("users")))

    val admin = if (groups(0).name.is("admin")) groups(0) else groups(1)
    val users = if (groups(0).name.is("users")) groups(0) else groups(1)
    admin.name should be (Ident("admin"))
    users.name should be (Ident("users"))
    admin.props should be (readOnly(Map("key" -> "value")))
    users.props should have size 0
    admin.rules should be (Set("bla.bla", "!bla.bla.blop"))
    users.rules should be (Set("resource:read:/**"))

    store.accounts should have size 1
    val john = store.accounts(0)._2
    john.name should be (Ident("john"))
    john.secrets should be (Seq(testpw))
    john.groups should be (Set("users", "admin").map(Ident.apply))
    john.props should be (readOnly(Map("enabled" -> "true")))
  }
}

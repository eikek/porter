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

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.store.{SimpleStore, StoreProvider}
import porter.model._
import porter.model.Account
import scala.util.{Failure, Success}
import scala.concurrent.{Await, ExecutionContext}

class AuthzTest extends FunSuite with ShouldMatchers {
  val testPw = Password("test")
  val factory = RuleFactory.providedFactory
  val realmId = Ident.randomIdent

  implicit val ec = ExecutionContext.global
  import scala.concurrent.duration._

  object TestAuth extends AuthZ with RuleFactory with StoreProvider {
    def store = new SimpleStore {
      def realms = List(Realm(realmId, ""))
      def accounts = List(realms(0) -> Account("john",
          Map("email" -> "john@mail.com", "enabled" -> "true"),
          Set("users", "admin"), Seq(testPw)))
      def groups = List(realms(0) ->
        Group(name = "admin", rules = Set("resource:read:/**", "contact:manage", "resource:read:/main/**"))
      )
    }
  }

  test("get policy") {
    val pol = Await.result(TestAuth.getPolicy(realmId, "john"), 5.seconds)
    pol.rules should have size (2)
    pol.grantsAll(List(factory("resource:read:/hello.html"))) should be (true)
    pol.grantsAll(List(factory("contact:delete"))) should be (false)
  }

  test("authorized") {
    val authz = (perms: List[Permission]) =>
      Await.result(TestAuth.authorized(realmId, "john", perms), 5.seconds)

    authz(List(factory("resource:read:/test.pdf"))) should be (true)
    authz(List(factory("contact:delete"))) should be (false)

    val Failure(_) = TestAuth.createRule("resource:read/test")
  }
}

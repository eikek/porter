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
import porter.model._
import porter.util.AES
import porter.model.Realm
import porter.model.Account

class DerivedValidatorTest extends FunSuite with ShouldMatchers {

  val realm = Realm(Ident.randomIdent, "")
  val aeskey = AES.deriveKey("superword", Vector(81.toByte))
  val secret = Password("test")

  test("authenticate") {
    val derived = DerivedCredentials("john", secret)
    val account = Account(name = "john", secrets = Seq(secret))
    val token = AuthToken(realm, account, Set(derived))
    val result = DerivedValidator.authenticate(token)
    result.votes should have size 1
    result.votes.get(secret.name).get should be (Vote.Success)
  }

  test("authenticate raw") {
    val data = DerivedCredentials("john", secret).encode(aeskey)
    val derived = DerivedCredentials.tryDecode(aeskey, data).get
    val account = Account(name = "john", secrets = Seq(secret))
    val token = AuthToken(realm, account, Set(derived))
    val result = DerivedValidator.authenticate(token)
    result.votes should have size 1
    result.votes.get(secret.name).get should be (Vote.Success)
  }
}

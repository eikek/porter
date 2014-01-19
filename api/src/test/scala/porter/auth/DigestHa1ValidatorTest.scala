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
import porter.util.Hash
import java.util.UUID

class DigestHa1ValidatorTest extends FunSuite with ShouldMatchers {

  import porter.model._
  import Secret._

  val uri = "/main/index.html"
  val method = "GET"
  val realm = Realm(Ident.randomIdent, "")
  val acc = Account(name = "john", secrets = Seq(DigestHa1("john", realm.name, "test")))

  def ha1(username: String, password: String) = Hash.md5String(username +":"+ realm.name +":"+ password)

  def clientResponse(ha1: String, nonce: String, qop: Option[DigestQop]) = {
    val ha2 = qop match {
      case Some(DigestAuthInt(_, _, body)) => Hash.md5String(method +":"+ uri +":"+ Hash.md5String(body))
      case _ => Hash.md5String(method +":"+ uri)
    }
    qop match {
      case Some(dqop) => Hash.md5String(ha1 +":"+ nonce +":"+ dqop.nonceCount +":"+ dqop.cnonce +":"+ dqop.name +":"+ ha2)
      case _ => Hash.md5String(ha1 +":"+ nonce +":"+ ha2)
    }
  }

  test("simple digest") {
    val nonce = Nonce.generateNonce()
    val response = clientResponse(ha1("john", "test"), nonce, None)
    val creds = DigestCredentials("john", method, uri, nonce, response)

    val token = AuthToken(realm, acc, Set(creds))
    val result = DigestValidator.authenticate(token)
    result.votes should have size 1
    result.votes("digestmd5.0") should be (Vote.Success)
  }

  test("digest with qop=auth") {
    val nonce = Nonce.generateNonce()
    val qop = DigestAuth(UUID.randomUUID().toString, "42")
    val response = clientResponse(ha1("john", "test"), nonce, Some(qop))
    val creds = DigestCredentials("john", method, uri, nonce, response, Some(qop))

    val token = AuthToken(realm, acc, Set(creds))
    val result = DigestValidator.authenticate(token)
    result.votes should have size 1
    result.votes("digestmd5.0") should be (Vote.Success)
  }

  test("nonce expired") {
    import scala.concurrent.duration._
    val nonce = Nonce.generateNonce(-1.millis)
    val response = clientResponse(ha1("john", "test"), nonce, None)
    val creds = DigestCredentials("john", method, uri, nonce, response)

    val token = AuthToken(Realm("testrealm", ""), acc, Set(creds))
    val result = DigestValidator.authenticate(token)
    result.votes should have size 1
    result.votes.get("digestmd5.0") should be (Some(DigestValidator.Reasons.nonceExpired))
  }
}
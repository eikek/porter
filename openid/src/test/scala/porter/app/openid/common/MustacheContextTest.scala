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

package porter.app.openid.common

import org.scalatest.FunSuite
import porter.app.openid.routes.Templating

class MustacheContextTest extends FunSuite {
  import MustacheContext._
  import Templating._

  val one = KeyedData("one")
  val two = KeyedData("two")
  val three = KeyedData("three")

  test("concatenate maps") {
    val c = Data.appendRaw(Map("three" -> 3))( one.put(1).andThen(two.put(2))(empty) )
    assert(c === Map("one" -> 1, "two" -> 2, "three" -> 3))
  }

  test("map conversion") {
    val c = KeyName.localId.put(LocalId("myrealm", "john"))
    assert(c(empty) === Map("localId" -> Map("realm" -> "myrealm", "account" -> "john")))
  }

  test("put optionals") {
    val localId: Option[LocalId] = None
    val someval: Option[String] = Some("value")
    val putId = KeyName.localId.put(localId)
    val putSome = KeyName.localId.put(someval)
    assert(putId(empty) === Map())
    assert(putSome(empty) === Map("localId" -> "value"))

    val novalue = TextField("email", "Email", None)
    val novalueF = one.put(novalue)
    val somevalue = TextField("email", "Email", Some("me@me.com"))
    val somevalueF = one.put(somevalue)
    assert(novalueF(empty) === Map("one" -> Map("name" -> "email", "label" -> "Email")))
    assert(somevalueF(empty) === Map("one" -> Map("name" -> "email", "label" -> "Email", "value" -> "me@me.com")))

    val fields = one.put(List(novalue, somevalue))
    assert(fields(empty) === Map[String, Any]("one" -> List(Map("name" -> "email", "label" -> "Email"),
      Map("name" -> "email", "label" -> "Email", "value" -> "me@me.com"))))
  }

}

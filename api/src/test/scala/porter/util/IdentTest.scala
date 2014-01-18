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

package porter.util

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.model.{ValidIdent, Ident}

class IdentTest extends FunSuite with ShouldMatchers {

  test("some valid identifiers") {
    Ident("abcs").name should be ("abcs")
    Ident("ab-cs").name should be ("ab-cs")
    Ident("90-ab_d").name should be ("90-ab_d")
    Ident("---").name should be ("---")
    Ident("_-_-_-").name should be ("_-_-_-")
    Ident("http://test.com").name should be ("http://test.com")
    Ident("me@mail.com").name should be ("me@mail.com")
  }

  test("convert identifier from a string") {
    Ident.convertString("90öabÄdä2++").get should be (Ident("90abd2"))
    Ident.convertString("+&+") should be (None)
  }

  test("random identifier") {
    //just call it several times and expect no exception
    for (i <- 1 to 200) Ident.randomIdent
  }

  test("isValid") {
    Ident.isValid("abc") should be (true)
    Ident.isValid("-abc") should be (true)
    Ident.isValid("a-bc") should be (true)
    Ident.isValid("abc-") should be (true)
    Ident.isValid("a_b_c-") should be (true)
    Ident.isValid("a*b_c-") should be (false)
    Ident.isValid("") should be (false)
  }

  test("unapply") {
    (Ident("hello") match {
      case Ident(x) => x
      case _ => "failed"
    }) should be ("hello")

    ("hello" match {
      case ValidIdent(z) => z
      case _ => "failed"
    }) should be ("hello")
  }
}

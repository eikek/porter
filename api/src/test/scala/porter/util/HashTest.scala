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

class HashTest extends FunSuite with ShouldMatchers {

  test("md5(test)") {
    Hash.md5String("test") should be ("098f6bcd4621d373cade4e832627b4f6")
  }

  test("sha1(test)") {
    Hash.sha1String("test") should be ("a94a8fe5ccb19ba61c4c0873d391e987982fbbd3")
  }

  test("sha256(test)") {
    Hash.sha256String("test") should be ("9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08")
  }

  test("sha384(test)") {
    Hash.sha384String("test") should be ("768412320f7b0aa5812fce428dc4706b3cae50e02a64caa16a782249bfe8efc4b7ef1ccb126255d196047dfedf17a0a9")
  }

  test("sha512(test)") {
    Hash.sha512String("test") should be ("ee26b0dd4af7e749aa1a8ee3c10ae9923f618980772e473f8819a5d4940e0db27ac185f8a0e1d5f84f88bc887fd67b143732c304cc5fa9ad8e6f57f50028a8ff")
  }
}

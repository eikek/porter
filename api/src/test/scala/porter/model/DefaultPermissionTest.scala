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

package porter.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class DefaultPermissionTest extends FunSuite with ShouldMatchers {

  test("empty permission") {
    val e = DefaultPermission(List())
    val p0 = DefaultPermission(List(Set("*")))
    e.implies(p0) should be (false)
    p0.implies(e) should be (true)
  }

  test("split ctor") {
    val p1 = DefaultPermission(List(Set("a"), Set("0"), Set("1")))
    val p2 = DefaultPermission(List(Set("a"), Set("0")))
    val p3 = DefaultPermission(List(Set("b"), Set("*")))
    val p4 = DefaultPermission(List(Set("b"), Set("k", "l", "m"), Set("0", "1")))

    p1 should be (DefaultPermission("a:0:1"))
    p2 should be (DefaultPermission("a:0"))
    p3 should be (DefaultPermission("b:*"))
    p4 should be (DefaultPermission("b:k,l,m:0,1"))

    //weird strings, but okay
    DefaultPermission(List(Set("b"), Set("c"))) should be (DefaultPermission("b:c,,,"))
    DefaultPermission(List(Set("b"), Set("c"), Set("d"))) should be (DefaultPermission("b:c,,:d"))
    DefaultPermission(List(Set("b"), Set("c"), Set("d"))) should be (DefaultPermission("b:,,c,,:d"))
  }

  test("invalid permission strings") {
    //dont know how to interpret empty parts, so these are errors
    intercept[IllegalArgumentException] {
      DefaultPermission("::bc")
    }

    intercept[IllegalArgumentException] {
      DefaultPermission(":::")
    }

    intercept[IllegalArgumentException] {
      DefaultPermission("b:")
    }
  }

  test("some simple implies") {
    val p1 = DefaultPermission("a:0:1")
    val p2 = DefaultPermission("a:0")
    val p3 = DefaultPermission("b:*")

    p1 implies p2 should be (false)
    p2 implies p1 should be (true)
    p1 implies p3 should be (false)
    p2 implies p3 should be (false)
    p3 implies DefaultPermission("b:n:x") should be (true)
  }
}

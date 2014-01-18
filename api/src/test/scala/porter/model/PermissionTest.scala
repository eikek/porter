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

class PermissionTest extends FunSuite with ShouldMatchers {

  import Permission._

  case class Perm(s: String) extends Permission {
    def implies(other: Permission) = other match {
      case sp: Perm => sp.s.toSet.subsetOf(s.toSet)
      case _ => false
    }
  }

  test("impliesAll") {
    val given = Seq(Perm("abc"), Perm("acdx"))
    val check = Seq(Perm("a"), Perm("b"), Perm("axx"))
    impliesAll(given, check) should be (true)
    impliesAll(given, check :+ Perm("az")) should be (false)
    impliesOne(given, check :+ Perm("axxxx")) should be (true)
  }

  test("impliesOne") {
    val given = Seq(Perm("abc"), Perm("acdx"))
    val check = Seq(Perm("a"), Perm("b"), Perm("axx"))
    impliesOne(given, check :+ Perm("az")) should be (true)
    impliesOne(given, Seq(Perm("7"), Perm("5"))) should be (false)
  }

  test("reduce") {
    val perms = Set(Perm("ab"), Perm("abcd"), Perm("a"), Perm("z"), Perm("abcde"), Perm("e"))
    reduce(perms) should be (Set(Perm("abcde"), Perm("z")))

    reduce(Set(Perm("a"), Perm("a"))) should be (Set(Perm("a")))
    reduce(Set(Perm("a"), Perm("aaa"))) should (be (Set(Perm("a"))) or be (Set(Perm("aaa"))))
    reduce(Set(Perm("ab"), Perm("ab"))) should be (Set(Perm("ab")))
  }
}

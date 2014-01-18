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
import porter.auth.RuleFactory

class PolicyTest extends FunSuite with ShouldMatchers {

  val policy = new Policy(Set(
    Permission("read:a:b:*"), //same as read:a:b
    Permission("read:z"),
    Permission("read:a:b:x").revoke
  ))

  test("grantsAll") {
    policy grantsAll List(Permission("read:a:b:c")) should be (true)
    policy grantsAll List(Permission("read:a:b:x:test")) should be (false)
    policy grantsAll List(Permission("read:z")) should be (true)
    policy grantsAll List(Permission("read:z:a")) should be (true)

    policy grantsAll List(Permission("read:z:a"), Permission("read:x:a")) should be (false)
  }

  test("reduce policy") {
    val fac = ResourcePermission.factory
    val policy = new Policy(Set(
      fac("resource:read:/**"),
      fac("resource:read:/main/**")
    ))
    policy.rules should have size (1)
  }

  test("reduce policy and revocations") {
    val fac = RuleFactory.providedFactory
    Policy(Set(fac("resource:read:/a/b/**"), fac("resource:read:/a/**").revoke)).rules should have size (0)
    Policy(Set(fac("a:b:*"), fac("a:x"), fac("a:x:1:n"), fac("a:x:1").revoke)).rules should have size (3)
  }
}

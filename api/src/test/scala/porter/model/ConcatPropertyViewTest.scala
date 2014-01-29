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

class ConcatPropertyViewTest extends FunSuite {

  test("concat properties") {
    import PropertyList._
    val props = firstName.set("Hugo").andThen(lastName.set("Klein"))(Map.empty)
    assert(fullName.get(props) === Some("Hugo Klein"))
  }
}

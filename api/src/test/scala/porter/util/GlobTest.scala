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

class GlobTest extends FunSuite with ShouldMatchers {

  case class G(path: String) {
    def matches(o: String) = Glob.matches(path, o)
  }

  test ("plain strings with no wildcards") {
    G("/ab/dd/e").matches("/ab/dd/e") should be (true)
    G("asda/sss/02").matches("asda/sss/02") should be (true)
    G("/ab/qq/d") matches "/ax/qq/d" should be (false)
    G("ab/d") matches "ab/de" should be (false)
  }

  test ("one-char placeholders") {
    G("/?b/d?/e").matches("/ab/dd/e") should be (true)
    G("/?b/d?/e").matches("/vb/d2/e") should be (true)
    G("/?b/d?/e").matches("/1b/dx/e") should be (true)
    G("/?b/d?/e").matches("/1b/xd/e") should be (false)

    G("asda/s??s/02").matches("asda/s2Ks/02") should be (true)
    G("asda/s??s/02").matches("asda/s2Kxs/02") should be (false)

    G("??ab/qq/d?") matches "siab/qq/d_" should be (true)
    G("??ab/qq/d?") matches "xsiab/qq/d_" should be (false)
  }

  test ("single star placeholders") {
    G("/dev/*/name.pdf") matches "/dev/hallo/name.pdf" should be (true)
    G("/dev/*/name.pdf") matches "/dev/hallo/priv/name.pdf" should be (false)
    G("/dev/*/*/name.pdf") matches "/dev/hallo/priv/name.pdf" should be (true)

    G("/dev/*/*.pdf") matches "/dev/hallo/name.pdf" should be (true)
    G("/dev/*/*.pdf") matches "/dev/hallo/new/name.pdf" should be (false)
    G("/dev/*/*/*.pdf") matches "/dev/hallo/new/name.pdf" should be (true)

    G("*/hallo/test") matches "/test/hallo/test" should be (true) //changed from old glob
    G("*/hallo/test") matches "test/hallo/test" should be (true)

    G("/hallo/test/*") matches "/hallo/test/" should be (true)
    G("/hallo/test/*") matches "/hallo/test/sdf" should be (true)
    G("/hallo/test/*") matches "/hallo/test/s22" should be (true)
    G("/hallo/test/*") matches "/hallo/test/s22.pdf" should be (true)
    G("/hallo/test/*") matches "/hallo/test/s22/h.pdf" should be (false)
  }

  test ("double char placeholders") {
    G("/hallo/**") matches "/hallo/test/s22/h.pdf" should be (true)
    G("/hallo/**/*.txt") matches "/hallo/test/s22/h.pdf" should be (false)
    G("/hallo/**/*.txt") matches "/hallo/test/s22/h.txt" should be (true) //changed from old glob
    G("/**") matches "/hallo/test/s22/h.pdf" should be (true)
    G("/**") matches "hallo/test/s22/h.pdf" should be (true) //changed from old glob
    G("**/name/*.pdf") matches "/hallo/test/s22/name/h.pdf" should be (true)
    G("**/name/*.pdf") matches "/hallo/test/s22/name/h.txt" should be (false)
    G("**/name/*") matches "/hallo/test/s22/name/h.txt" should be (true)
    G("**/name/*") matches "/hallo/test/s22/name/dev/h.txt" should be (false)
  }

  test ("simple implies") {
    G("/dev/**").matches("/dev/ab/**") should be (true)
    G("/dev/**").matches("/ab/**") should be (false)

    G("/?b/home/**").matches("/ab/home/tests/**") should be (true)
    G("/?b/home/**").matches("/ab/home/**") should be (true)

    G("/ab/*/c").matches("/?b/*/c") should be (false)
    G("/a?/*/c").matches("/ab/*/c") should be (true)

    G("**/name/*.pdf").matches("/one/two/name/**") should be (false)
    G("/*/*/name/*.pdf").matches("/one/two/name/*.pdf") should be (true)
    G("**/name/*.pdf").matches("/one/name/*.pdf") should be (true)

    G("/aa/bb/cc").matches("/aa/bb/cc") should be (true)
    G("/aa/bb/cc").matches("/aa/abb/cc") should be (false)

    G("/**").matches("**") should be (true) //changed from old glob
    G("**").matches("**") should be (true)
    G("/**").matches("/") should be (true)

    G("/help/**/*.pdf") matches "/help/a/b/c/test.pdf" should be (true)
    G("/help/**/*.pdf") matches "/help/a/b/c/test.txt" should be (false)
  }
}

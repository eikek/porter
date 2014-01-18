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

/**
 * Decides for a given [[porter.auth.AuthResult]] if it represents
 * a successful authentication.
 *
 */
trait Decider extends (AuthResult => Boolean)

/**
 * Returns `true` if there is at least one successful vote and
 * no failed ones. It also checks the properties
 * [[porter.model.PropertyList.accountDisabled]] and [[porter.model.PropertyList.accountLocked]].
 */
object OneSuccessfulVote extends Decider {
  import Decider._
  def apply(result: AuthResult) =
    notDisabled(result) && result.successCount > 0 && result.failedCount == 0
}

/**
 * Returns `true` if there is at least one successful vote while
 * it does not look at any failed ones. It also checks the properties
 * [[porter.model.PropertyList.accountDisabled]] and [[porter.model.PropertyList.accountLocked]].
 */
object SomeSuccessfulVote extends Decider {
  import Decider._
  def apply(result: AuthResult) = notDisabled(result) && result.successCount > 0
}

object Decider {
  import porter.model.PropertyList._

  def isDisabled(result: AuthResult) =
    accountDisabled.isTrue(result.props) || accountLocked.isTrue(result.props)

  def notDisabled(result: AuthResult) = !isDisabled(result)

}

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

import porter.model.Ident

@SerialVersionUID(20131122)
sealed trait Vote extends Serializable {
  def toBoolean: Boolean
  def isFailed: Boolean
  def isSuccess: Boolean
}

object Vote {
  @SerialVersionUID(20131122)
  case object Success extends Vote {
    val toBoolean = true
    val isFailed = false
    val isSuccess = true
  }

  @SerialVersionUID(20131122)
  final case class Failed(reasons: Map[Ident, String] = Map.empty) extends Vote {
    val toBoolean = false
    val isFailed = true
    val isSuccess = false
  }

  object Failed {
    def apply(values: (Ident, String)*): Failed = apply(values.toMap)
  }
}
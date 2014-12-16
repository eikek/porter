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

import porter.model._
import porter.model.Account
import porter.model.Realm

@SerialVersionUID(20131122)
final case class AuthToken(realm: Realm,
                           account: Account,
                           credentials: Set[Credentials],
                           votes: Map[Ident, Vote] = Map.empty) extends Serializable {

  def vote(v: (Ident, Vote)) = copy(votes = votes + (v._1 -> v._2))

  def toResult = AuthResult(realm, account.name, votes, account.props)
}

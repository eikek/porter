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

package porter.app.client

import porter.auth.{OneSuccessfulVote, Decider}
import porter.model._
import porter.app.akka.PorterRef
import porter.client.messages._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

case class PorterContext(porterRef: PorterRef, realm: Ident, decider: Decider = OneSuccessfulVote) {
  val client = new PorterAkkaClient(porterRef, decider)

  def findAccounts(names: Set[Ident])(implicit ec: ExecutionContext, timeout: FiniteDuration) =
    client.findAccounts(FindAccounts(realm, names))

  def updateAccount(account: Account)(implicit ec: ExecutionContext, timeout: FiniteDuration) =
    client.updateAccount(UpdateAccount(realm, account))

  def updateGroup(group: Group)(implicit ec: ExecutionContext, timeout: FiniteDuration) =
    client.updateGroup(UpdateGroup(realm, group))

  def createNewAccount(account: Account)(implicit ec: ExecutionContext, timeout: FiniteDuration) =
    client.createNewAccount(UpdateAccount(realm, account))

  def changeSecrets(creds: Set[Credentials], secrets: List[Secret])(implicit ec: ExecutionContext, timeout: FiniteDuration) =
    client.changeSecrets(ChangeSecrets(realm, creds, secrets))
}

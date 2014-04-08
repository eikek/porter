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
import porter.app.akka.{PorterUtil, PorterRef}
import porter.client.messages._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration.FiniteDuration
import porter.client.json.MessageJsonProtocol.UserPass
import akka.util.Timeout

case class PorterContext(porterRef: PorterRef, realm: Ident, decider: Decider = OneSuccessfulVote) {
  val client = new PorterAkkaClient(porterRef, decider)

  def findAccounts(names: Set[Ident])(implicit ec: ExecutionContext, timeout: Timeout) =
    client.findAccounts(FindAccounts(realm, names))(ec, timeout.duration)


  def findGroups(names: Set[Ident])(implicit ec: ExecutionContext, timeout: Timeout) =
    client.findGroups(FindGroups(realm, names))(ec, timeout.duration)

  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext, timeout: Timeout) =
    client.findRealms(FindRealms(names))(ec, timeout.duration)

  def listAccounts()(implicit ec: ExecutionContext, timeout: Timeout) =
    client.listAccounts(GetAllAccounts(realm))(ec, timeout.duration)

  def listGroups()(implicit ec: ExecutionContext, timeout: Timeout) =
    client.listGroups(GetAllGroups(realm))(ec, timeout.duration)

  def updateAccount(account: Account)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.updateAccount(UpdateAccount(realm, account))(ec, timeout.duration)

  def updateGroup(group: Group)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.updateGroup(UpdateGroup(realm, group))(ec, timeout.duration)

  def createNewAccount(account: Account)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.createNewAccount(UpdateAccount(realm, account))(ec, timeout.duration)

  def updateRealm(newrealm: Realm)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.updateRealm(UpdateRealm(newrealm))(ec, timeout.duration)

  def deleteAccount(name: Ident)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.deleteAccount(DeleteAccount(realm, name))(ec, timeout.duration)

  def deleteGroup(name: Ident)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.deleteGroup(DeleteGroup(realm, name))(ec, timeout.duration)

  def deleteRealm(name: Ident)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.deleteRealm(DeleteRealm(name))(ec, timeout.duration)

  def changeSecrets(creds: Set[Credentials], secrets: List[Secret])(implicit ec: ExecutionContext, timeout: Timeout) =
    client.changeSecrets(ChangeSecrets(realm, creds, secrets))(ec, timeout.duration)

  def authenticate(creds: Set[Credentials])(implicit ec: ExecutionContext, timeout: Timeout) =
    client.authenticate(Authenticate(realm, creds))(ec, timeout.duration)

  def authenticateAccount(creds: Set[Credentials])(implicit ec: ExecutionContext, timeout: Timeout) =
    client.authenticateAccount(Authenticate(realm, creds))(ec, timeout.duration)

  def authenticateSimple(creds: UserPass)(implicit ec: ExecutionContext, timeout: Timeout) =
    client.authenticateSimple(realm)(creds)(ec, timeout.duration)

  def authorize(account: Ident, perms: Set[String])(implicit ec: ExecutionContext, timeout: Timeout): Future[AuthorizeResp] =
    client.authorize(Authorize(realm, account, perms))(ec, timeout.duration)

  def authorize(account: Ident, perm: String, more: String*)(implicit ec: ExecutionContext, timeout: Timeout): Future[AuthorizeResp] =
    authorize(account, (more :+ perm).toSet)(ec, timeout.duration)

  def updateAccount(name: Ident, alter: Account => Account)(implicit ec: ExecutionContext, timeout: Timeout) =
    PorterUtil.updateAccount(porterRef, realm, name, alter)

  def updateGroup(name: Ident, alter: Group => Group)(implicit ec: ExecutionContext, timeout: Timeout) =
    PorterUtil.updateGroup(porterRef, realm, name, alter)
  
  def updateRealm(realmId: Ident, alter: Realm => Realm)(implicit ec: ExecutionContext, timeout: Timeout) =
    PorterUtil.updateRealm(porterRef, realmId, alter)
}

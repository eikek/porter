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

package porter.client.http

import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext
import porter.client.json.MessageJsonProtocol
import porter.client.PorterClient
import porter.client.messages._
import scala.concurrent.duration.FiniteDuration
import porter.model.Ident

/**
 * Simple rest client providing basic porter functions. Note that "spray-json" dependency is
 * required for this and it is not included by default. You will need to provide the "spray-json"
 * artefacts yourself.
 *
 * @param addr
 */
class PorterHttp(addr: InetSocketAddress) extends PorterClient {
  def this() = this(new InetSocketAddress("localhost", 6789))

  import spray.json._
  import MessageJsonProtocol._

  private def post[A](path: String, data: A)(implicit ec: ExecutionContext, timeout: FiniteDuration, rfa: RootJsonFormat[A]) =
    Http.post(addr, path, data.toJson.compactPrint)

  private def perform[A, B](path: String)(implicit rfa: RootJsonFormat[A], rfb: RootJsonFormat[B]) =
    new Command[A, B] {
      def apply(req: A)(implicit ec: ExecutionContext, timeout: FiniteDuration) = {
        post(path, req).map(_.parseJson.convertTo[B])
      }
    }

  private def modifyCmd[A](path: String)(implicit rf: RootJsonFormat[A]) = perform[A, OperationFinished](path)

  def authenticate = perform[Authenticate, AuthenticateResp]("/api/authc")
  def authenticateAccount = perform[Authenticate, AuthAccount]("/api/authc/account")
  def authenticateSimple(realm: Ident) = perform[UserPass, SimpleAuthResult]("/api/authc/simple/"+realm.name)

  def authorize = perform[Authorize, AuthorizeResp]("/api/authz")
  def retrieveNonce = perform[RetrieveServerNonce, RetrieveServerNonceResp]("/api/authc/serverNonce")


  def listAccounts = perform[GetAllAccounts, FindAccountsResp]("/api/account/all")
  def findAccounts = perform[FindAccounts, FindAccountsResp]("/api/account/find")
  def listGroups = perform[GetAllGroups, FindGroupsResp]("/api/group/all")
  def findGroups = perform[FindGroups, FindGroupsResp]("/api/group/find")
  def findRealms = perform[FindRealms, FindRealmsResp]("/api/realm/find")

  def updateAccount = modifyCmd[UpdateAccount]("/api/account/update")
  def createNewAccount = modifyCmd[UpdateAccount]("/api/account/new")
  def updateGroup = modifyCmd[UpdateGroup]("/api/group/update")
  def updateRealm = modifyCmd[UpdateRealm]("/api/realm/update")
  def deleteAccount = modifyCmd[DeleteAccount]("/api/account/delete")
  def deleteGroup = modifyCmd[DeleteGroup]("/api/group/delete")
  def deleteRealm = modifyCmd[DeleteRealm]("/api/realm/delete")
  def changeSecrets = modifyCmd[ChangeSecrets]("/api/account/changeSecrets")
  def updateAuthProps = modifyCmd[UpdateAuthProps]("/api/account/updateAuthProps")
}

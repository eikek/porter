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

package porter.client

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

trait PorterClient {

  import Messages.store._
  import Messages.mutableStore._
  import Messages.auth._

  trait Command[A, B] {
    def apply(req: A)(implicit ec: ExecutionContext, timeout: FiniteDuration): Future[B]
  }

  type FindAccountCmd = Command[FindAccounts, FindAccountsResp]
  type FindGroupCmd = Command[FindGroups, FindGroupsResp]
  type FindRealmCmd = Command[FindRealms, FindRealmsResp]

  def findAccounts: FindAccountCmd
  def findGroups: FindGroupCmd
  def findRealms: FindRealmCmd

  type UpdateAccountCmd = Command[UpdateAccount, OperationFinished]
  type UpdateGroupCmd = Command[UpdateGroup, OperationFinished]
  type UpdateRealmCmd = Command[UpdateRealm, OperationFinished]
  type DeleteAccountCmd = Command[DeleteAccount, OperationFinished]
  type DeleteGroupCmd = Command[DeleteGroup, OperationFinished]
  type DeleteRealmCmd = Command[DeleteRealm, OperationFinished]
  type ChangePasswordCmd = Command[ChangePassword, OperationFinished]
  type UpdateAuthPropsCmd = Command[UpdateAuthProps, OperationFinished]

  def updateAccount: UpdateAccountCmd
  def createNewAccount: UpdateAccountCmd
  def updateGroup: UpdateGroupCmd
  def updateRealm: UpdateRealmCmd
  def deleteAccount: DeleteAccountCmd
  def deleteGroup: DeleteGroupCmd
  def deleteRealm: DeleteRealmCmd
  def changePassword: ChangePasswordCmd
  def updateAuthProps: UpdateAuthPropsCmd

  type AuthcCmd = Command[Authenticate, AuthenticateResp]
  type AuthcAccountCmd = Command[Authenticate, AuthAccount]
  type AuthzCmd = Command[Authorize, AuthorizeResp]
  type ServerNonceCmd = Command[RetrieveServerNonce, RetrieveServerNonceResp]

  def authenticate: AuthcCmd
  def authenticateAccount: AuthcAccountCmd
  def authorize: AuthzCmd
  def retrieveNonce: ServerNonceCmd
}

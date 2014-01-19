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

package porter.client.json

import porter.client.Messages.store._
import porter.client.Messages.mutableStore._
import porter.client.Messages.auth._
import porter.auth.AuthResult
import spray.json._
import scala.concurrent.duration.Duration

trait MessageJsonProtocol extends ModelJsonProtocol {

  implicit val findAccountFormat = jsonFormat2(FindAccounts)
  implicit val findAccountRespFormat = jsonFormat1(FindAccountsResp)

  implicit val findGroupsFormat = jsonFormat2(FindGroups)
  implicit val findGroupsRespFormat = jsonFormat1(FindGroupsResp)

  implicit val findRealmsFormat = jsonFormat1(FindRealms)
  implicit val findRealmsRespFormat = jsonFormat1(FindRealmsResp)

  implicit val operationFinishedFormat = jsonFormat1(OperationFinished)
  implicit val getAllAccountsFormat = jsonFormat(GetAllAccounts, "realm")
  implicit val updateAccountFormat = jsonFormat(UpdateAccount, "realm", "account")
  implicit val deleteAccountFormat = jsonFormat(DeleteAccount, "realm", "account")
  implicit val getAllGroupsFormat = jsonFormat(GetAllGroups, "realm")
  implicit val updateGroupFormat = jsonFormat(UpdateGroup, "realm", "group")
  implicit val deleteGroupFormat = jsonFormat(DeleteGroup, "realm", "group")
  implicit val updateRealmFormat = jsonFormat(UpdateRealm, "realm")
  implicit val deleteRealmFormat = jsonFormat(DeleteRealm, "realm")
  implicit val changeSecretsFormat = jsonFormat3(ChangeSecrets)

  implicit val authcFormat = jsonFormat(Authenticate, "realm", "creds")
  implicit val authresultFormat = jsonFormat(AuthResult, "realm", "accountId", "votes", "props")
  implicit val authcResponse = jsonFormat1(AuthenticateResp)
  implicit val authAccountFormat = jsonFormat2(AuthAccount)

  implicit val authzFormat = jsonFormat3(Authorize)
  implicit val authzResponseFormat = jsonFormat3(AuthorizeResp)

  implicit val updateAuthPropFormat = jsonFormat3(UpdateAuthProps)

  implicit object DurationFormat extends JsonFormat[Duration] {
    def write(obj: Duration) = JsString(obj.toString)
    def read(json: JsValue) = json match {
      case JsString(value) => Duration(value)
      case x => deserializationError("Unable to get duration for string "+ x)
    }
  }
  implicit val retrieveServerNonceFormat = jsonFormat1(RetrieveServerNonce)
  implicit val retrieveServerNonceRespFormat = jsonFormat1(RetrieveServerNonceResp)
}

object MessageJsonProtocol extends MessageJsonProtocol

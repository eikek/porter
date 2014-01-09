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
  implicit val updateAccountFormat = jsonFormat(UpdateAccount, "realm", "account")
  implicit val deleteAccountFormat = jsonFormat(DeleteAccount, "realm", "account")
  implicit val updateGroupFormat = jsonFormat(UpdateGroup, "realm", "group")
  implicit val deleteGroupFormat = jsonFormat(DeleteGroup, "realm", "group")
  implicit val updateRealmFormat = jsonFormat(UpdateRealm, "realm")
  implicit val deleteRealmFormat = jsonFormat(DeleteRealm, "realm")
  implicit val changePasswordFormat = jsonFormat3(ChangePassword)

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

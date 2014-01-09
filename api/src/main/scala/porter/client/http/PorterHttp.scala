package porter.client.http

import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext
import porter.client.json.MessageJsonProtocol
import porter.client.PorterClient
import porter.client.Messages.store._
import porter.client.Messages.mutableStore._
import porter.client.Messages.auth._

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

  private def post[A](path: String, data: A)(implicit ec: ExecutionContext, rfa: RootJsonFormat[A]) =
    Http.post(addr, path, data.toJson.compactPrint)

  private def perform[A, B](path: String)(implicit rfa: RootJsonFormat[A], rfb: RootJsonFormat[B]) =
    new Command[A, B] {
      def apply(req: A)(implicit ec: ExecutionContext) = {
        post(path, req).map(_.asJson.convertTo[B])
      }
    }

  private def modifyCmd[A](path: String)(implicit rf: RootJsonFormat[A]) = perform[A, OperationFinished](path)

  def authenticate = perform[Authenticate, AuthenticateResp]("/api/authc")
  def authenticateAccount = perform[Authenticate, AuthAccount]("/api/authcAccount")
  def authorize = perform[Authorize, AuthorizeResp]("/api/authz")

  def findAccounts = perform[FindAccounts, FindAccountsResp]("/api/account/find")
  def findGroups = perform[FindGroups, FindGroupsResp]("/api/group/find")
  def findRealms = perform[FindRealms, FindRealmsResp]("/api/realm/find")

  def updateAccount = modifyCmd[UpdateAccount]("/api/account/update")
  def createNewAccount = modifyCmd[UpdateAccount]("/api/account/new")
  def updateGroup = modifyCmd[UpdateGroup]("/api/group/update")
  def updateRealm = modifyCmd[UpdateRealm]("/api/realm/update")
  def deleteAccount = modifyCmd[DeleteAccount]("/api/account/delete")
  def deleteGroup = modifyCmd[DeleteGroup]("/api/group/delete")
  def deleteRealm = modifyCmd[DeleteRealm]("/api/realm/delete")
  def changePassword = modifyCmd[ChangePassword]("/api/account/changePassword")
  def updateAuthProps = modifyCmd[UpdateAuthProps]("/api/account/updateAuthProps")
}

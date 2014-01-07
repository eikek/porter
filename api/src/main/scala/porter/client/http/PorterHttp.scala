package porter.client.http

import java.net.InetSocketAddress
import scala.concurrent.{Future, ExecutionContext}
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

  private implicit class FutureUnmarshall(f: Future[String]) {
    def convertTo[A](implicit ec: ExecutionContext, rf: RootJsonFormat[A]): Future[A] =
      f.map(_.toJson.convertTo[A])
  }
  private def post[A](path: String, data: A)(implicit ec: ExecutionContext, rfa: RootJsonFormat[A]) =
    Http.post(addr, path, rfa.write(data).compactPrint)


  def authenticate = new AuthcCmd {
    def apply(req: Authenticate)(implicit ec: ExecutionContext) = {
      val path = "/api/authc"
      post(path, req).convertTo[AuthenticateResp]
    }
  }

  def authenticateAccount = new AuthcAccountCmd {
    def apply(req: Authenticate)(implicit ec: ExecutionContext) = {
      val path = "/api/authcAccount"
      post(path, req).convertTo[AuthAccount]
    }
  }

  def authorize = new AuthzCmd {
    def apply(req: Authorize)(implicit ec: ExecutionContext) = {
      val path = "/api/authz"
      post(path, req).convertTo[AuthorizeResp]
    }
  }

  def findAccounts = new FindAccountCmd {
    def apply(req: FindAccounts)(implicit ec: ExecutionContext) = {
      val path = "/api/account/find"
      post(path, req).convertTo[FindAccountsResp]
    }
  }

  def findGroups = new FindGroupCmd {
    def apply(req: FindGroups)(implicit ec: ExecutionContext) = {
      val path = "/api/group/find"
      post(path, req).convertTo[FindGroupsResp]
    }
  }

  def findRealms = new FindRealmCmd {
    def apply(req: FindRealms)(implicit ec: ExecutionContext) = {
      val path = "/api/realm/find"
      post(path, req).convertTo[FindRealmsResp]
    }
  }

  private def modifyCmd[A](path: String)(implicit rf: RootJsonFormat[A]) = new Command[A, OperationFinished] {
    def apply(req: A)(implicit ec: ExecutionContext) = {
      post(path, req).convertTo[OperationFinished]
    }
  }

  def updateAccount = modifyCmd[UpdateAccount]("/api/account/update")
  def createNewAccount = modifyCmd[UpdateAccount]("/api/account/new")
  def updateGroup = modifyCmd[UpdateGroup]("/api/group/update")
  def updateRealm = modifyCmd[UpdateRealm]("/api/realm/update")
  def deleteAccount = modifyCmd[DeleteAccount]("/api/account/delete")
  def deleteGroup = modifyCmd[DeleteGroup]("/api/group/delete")
  def deleteRealm = modifyCmd[DeleteRealm]("/api/realm/delete")
  def changePassword = modifyCmd[ChangePassword]("/api/account/changePassword")
}

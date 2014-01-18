package porter.app.openid.routes.manage

import porter.app.openid.routes.OpenIdActors
import porter.model.PropertyList
import porter.client.Messages.mutableStore.{OperationFinished, UpdateAccount}
import spray.routing.Directives._
import scala.util.Success
import akka.pattern.ask

trait DeleteAvatar {
  self: ManageRoutes with OpenIdActors =>

  def deleteAvatar: Submission = {
    case Action("deleteAvatar", ctx, acc) =>
      val nacc = acc.updatedProps(PropertyList.avatar.remove)
      val upd = UpdateAccount(settings.defaultRealm, nacc)
      onComplete((porterRef ? upd).mapTo[OperationFinished]) {
        case Success(OperationFinished(true)) => renderUserPage(nacc, Message.success("Account saved."))
        case _ => renderUserPage(acc, Message.error("Error saving account."))
      }
  }

}

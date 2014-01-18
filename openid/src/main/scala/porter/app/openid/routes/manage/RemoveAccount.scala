package porter.app.openid.routes.manage

import porter.app.openid.routes.OpenIdActors
import spray.routing.Directives._
import porter.client.Messages.mutableStore.OperationFinished
import porter.client.Messages.mutableStore.DeleteAccount
import scala.Some
import scala.util.{Failure, Success}
import spray.http.StatusCodes
import akka.pattern.ask

trait RemoveAccount {
  self: ManageRoutes with OpenIdActors =>

  def deleteAccount: Submission = {
    case Action("removeAccount", ctx, acc) =>
      paramOpt("porter.removeAccount") { p =>
        if (p == Some("on")) {
          log.info(s"About to delete account '${acc.name.name}'.")
          val f = (porterRef ? DeleteAccount(settings.defaultRealm, acc.name)).mapTo[OperationFinished]
          onComplete(f) {
            case Success(of) if of.result =>
              log.info(s"Account '${acc.name.name}' deleted.")
              removePorterCookie() {
                redirectToUserPage
              }
            case Success(of) =>
              log.error(s"Failed to delete account '${acc.name.name}")
              renderUserPage(acc, Message.info("Could not remove account."))
            case Failure(ex) =>
              log.error(ex, s"Failed to delete account '${acc.name.name}")
              renderUserPage(acc, Message.error("Could not remove account: " +ex.getMessage))
          }
        } else {
          renderUserPage(acc, Message.info("Please enable the check box to allow deletion."))
        }
      }

  }
}

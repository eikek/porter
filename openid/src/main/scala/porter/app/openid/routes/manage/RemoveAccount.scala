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
      formField("porter.removeAccount".?) { p =>
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

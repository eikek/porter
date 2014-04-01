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
import porter.model.PropertyList
import porter.client.messages._
import scala.util.Success
import akka.pattern.ask

trait DeleteAvatar {
  self: ManageRoutes with OpenIdActors =>

  def deleteAvatar: Submission = {
    case Action("deleteAvatar", ctx, acc) =>
      val nacc = acc.updatedProps(PropertyList.avatar.remove)
      val upd = UpdateAccount(settings.defaultRealm, nacc)
      onComplete((porterRef ? upd).mapTo[OperationFinished]) {
        case Success(OperationFinished.success) => renderUserPage(nacc, Message.success("Account saved."))
        case _ => renderUserPage(acc, Message.error("Error saving account."))
      }
  }

}

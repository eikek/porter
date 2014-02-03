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

import spray.http._
import porter.app.openid.routes.{Templating, OpenIdActors, AuthDirectives, BasicDirectives}
import spray.http.HttpResponse

trait PageDirectives extends BasicDirectives with AuthDirectives {
  self: OpenIdActors =>

  import PageDirectives._

  private def loginPage(failed: Boolean) = {
    import Templating._
    val data = KeyName.loginFailed.put(failed)
      .andThen(KeyName.registerUrl.putIf(settings.registrationEnabled,
      settings.endpointBaseUrl.toRelative.toString()+"?register"))

    val context = data(defaultContext(settings))
    settings.loginTemplate(context)
  }


  private def errorPage(params: Map[String, String]) = {
    import Templating._
    val context = KeyName.params.putPairList(params)(defaultContext(settings))
    settings.errorTemplate(context.toMap)
  }

  def renderLoginPage(failed: Boolean) = removePorterCookie() {
    complete(HttpEntity(html, loginPage(failed)))
  }

  def renderErrorPage = complete {
    HttpResponse(
      status = StatusCodes.BadRequest,
      entity = HttpEntity(html, errorPage(Map.empty)))
  }

}

object PageDirectives {

  val html = ContentType(MediaTypes.`text/html`)

}
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

package porter.app.openid.routes

import porter.app.openid.common.{RequestedAttributes, MustacheContext, Keys, LocalId}
import spray.routing.Directives._
import spray.http._
import spray.http.HttpResponse
import java.util.Locale
import porter.model.Account
import porter.app.openid.routes.Templating.AttributeValues

trait PageDirectives extends OpenIdDirectives with AuthDirectives {
  self: OpenIdActors =>

  import PageDirectives._

  private def loginPage(params: Map[String, String], lid: Option[LocalId], endpointUrl: Uri, failed: Boolean) = {
    import Templating._
    val data = Keys.realm.put(params.get(Keys.realm.openid))
      .andThen(Keys.identity.put(params.get(Keys.identity.openid)))
      .andThen(KeyName.endpointUrl.put(endpointUrl.toRelative))
      .andThen(KeyName.localId.put(lid))
      .andThen(KeyName.loginFailed.put(failed))
      .andThen(KeyName.params.putPairList(params))
      .andThen(KeyName.registerUrl.putIf(settings.registrationEnabled, settings.endpointBaseUrl.toRelative.toString()+"?register"))

    val context = data(defaultContext(settings))
    settings.loginTemplate(context)
  }


  private def continueForm(returnto: Uri, params: Map[String, String], attr: AttributeValues) = {
    import MustacheContext._
    import Templating._
    val rto = returnto.copy(query = Uri.Query.Empty)
    val data = Keys.realm.put(params.get(Keys.realm.openid))
      .andThen(Keys.identity.put(params.get(Keys.identity.openid)))
      .andThen(KeyName.endpointUrl.put(settings.endpointUrl.toRelative))
      .andThen(KeyName.returnToUrl.put(rto))
      .andThen(KeyName.params.putPairList(params ++ returnto.query.toMap))
      .andThen(Data.append(attr))

    val context = data(defaultContext(settings))
    settings.continueTemplate(context)
  }

  private def errorPage(params: Map[String, String]) = {
    import Templating._
    val context = KeyName.params.putPairList(params)(defaultContext(settings))
    settings.errorTemplate(context.toMap)
  }

  def renderLoginPage(endpointUrl: Uri, failed: Boolean) = allParams { req =>
    localIdOption { lidopt =>
      removePorterCookie() {
        complete(HttpResponse(
          entity = HttpEntity(html,
            loginPage(req, lidopt, endpointUrl, failed))))
      }
    }
  }

  def renderErrorPage = allParams { params =>
    complete(HttpResponse(
      status = StatusCodes.BadRequest,
      entity = HttpEntity(html, errorPage(params))))
  }

  def renderContinuePage(account: Account, params: Map[String, String]) = returnToUrl { uri =>
    val localid = params.get(Keys.identity.openid) match {
      case Some(LocalIdParts(lid)) => lid
      case _ => sys.error("Invalid positive assertion! Does not contain openid.identity attribute")
    }
    val paramsWithId = params
      .updated("porter.realm", localid.realm.name)
      .updated("porter.account", localid.account.name)
    sRegExtensionFields { attr =>
      localeWithDefault(Some(account)) { loc =>
        complete(HttpResponse(
          entity = HttpEntity(html, continueForm(uri, paramsWithId, AttributeValues(attr, account, loc)))))
      }
    }

  } ~ renderErrorPage

}

object PageDirectives {

  val html = ContentType(MediaTypes.`text/html`)

}
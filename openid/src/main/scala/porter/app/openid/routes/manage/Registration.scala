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
import spray.http._
import porter.app.openid.OpenIdServiceSettings
import porter.model.{PropertyList, Password, Ident}
import spray.routing._
import porter.app.akka.PorterUtil
import akka.pattern.ask
import spray.http.MediaTypes._
import spray.httpx.unmarshalling._
import spray.http.HttpRequest
import scala.util.Failure
import porter.model.Account
import scala.util.Success
import porter.client.Messages.store.{FindAccountsResp, FindAccounts}

trait Registration {
  self: ManageRoutes with OpenIdActors =>

  import Registration._

  def registrationRoute: Route = {
    entity(as[RegistrationReq]) { reg =>
      invalidRegistration(reg) { errors =>
        renderRegistrationPage(reg.fields, errors)
      } ~
      createAccount(reg) { acc =>
        setPorterCookieOnRememberme(acc) {
          redirectToUserPage
        }
      } ~ renderErrorPage
    } ~ renderRegistrationPage(Map.empty, List("Invalid registration request."))
  }

  private def invalidRegistration(reg: RegistrationReq): Directive1[List[String]] = {
    val errors = reg.check(settings)
    if (errors.nonEmpty) provide(errors) else {
      val f = for {
        resp <- (porterRef ? FindAccounts(settings.defaultRealm, Set(reg.name))).mapTo[FindAccountsResp]
      } yield resp.accounts.isEmpty
      onComplete(f).flatMap {
        case Success(true) => reject()
        case _ => provide(List("Account name already in use."))
      }
    }
  }

  private def createAccount(reg: RegistrationReq): Directive1[Account] =
    onComplete(PorterUtil.createNewAccount(porterRef, settings.defaultRealm, reg.toAccount(settings))).flatMap {
      case Success(account) => provide(account)
      case Failure(ex) => log.error(ex, "Unable to create account!"); reject()
    }
}

object Registration {
  case class RegistrationReq(name: String,
                          email: Option[String],
                          password1: String,
                          password2: String,
                          key: Option[String],
                          fields: Map[String, String]) {

    def check(settings: OpenIdServiceSettings): List[String] = {
      if (!settings.registrationEnabled) List("Registration disabled")
      else {
        List(
          if (Ident.fromString(name).isDefined) "" else "Account name is invalid.",
          if (Ident.fromString(name) != Some(Ident("openid"))) "" else "The account name 'openid' cannot be used.",
          if (password1.trim.nonEmpty && password1 == password2) "" else "Password is required and both must match.",
          if (settings.registrationKey == key) "" else "Registration key is invalid.",
          if (!settings.registrationRequiresEmail || email.exists(_.indexOf('@') > 0)) "" else "Valid email is required."
        ).filter(_.nonEmpty)
      }
    }

    def toAccount(settings: OpenIdServiceSettings): Account = {
      val a = Account(name = name, secrets = Seq(Password(settings.passwordCrypt)(password1)))
      email.map(PropertyList.email.set).map(a.updatedProps).getOrElse(a)
    }
  }

  object RegistrationReqUnmarshaller extends SimpleUnmarshaller[RegistrationReq] {
    import spray.httpx.unmarshalling._
    val canUnmarshalFrom = ContentTypeRange(`application/x-www-form-urlencoded`) :: Nil
    val names = List("porter.username", "porter.email", "porter.password1", "porter.password2", "porter.registerKey")

    protected def unmarshal(entity: HttpEntity) = {
      entity.as[FormData].right.map(_.fields.toMap).right.flatMap(extract)
    }

    private def extract(data: Map[String, String]) = {
      val values = names.map(k => data.get(k))
      values match {
        case Some(name) :: email :: Some(pw1) :: Some(pw2) :: key :: Nil =>
          Right(RegistrationReq(name, email, pw1, pw2, key, data.filterNot(_._1.startsWith("porter.password"))))
        case _ => Left(MalformedContent("Invalid registration request"))
      }
    }
  }

  implicit val registrationRequestUM: FromRequestUnmarshaller[RegistrationReq] = new FromRequestUnmarshaller[RegistrationReq] {
    def apply(req: HttpRequest) = RegistrationReqUnmarshaller(req.entity)
  }

}

package porter.app.openid.routes

import spray.routing.{Directive1, PathMatchers, Directives, Route}
import spray.http._
import spray.http.HttpResponse
import porter.model.{Password, Ident, Account}
import porter.app.akka.api.MutableStoreActor.messages.{OperationFinished, UpdateAccount}
import scala.util.{Failure, Try, Success}

trait HomeRoutes {
  self: OpenIdDirectives with PageDirectives with AuthDirectives =>

  import Directives._
  import HomeRoutes._
  import PageDirectives._
  import akka.pattern.ask

  type Action = PartialFunction[(String, Account), Route]

  private lazy val actions: Action = {
    case ("changeSecret", acc) =>
      updateSecretSubmit(settings.defaultRealm, acc) {
        case Success(nacc) =>
          val context = Map("infoMessage" -> Map("level" -> "success", "message" -> "Secret changed."))
          setPorterCookieOnRememberme(nacc) {
            renderUserPage(nacc, context)
          }
        case Failure(ex) =>
          val context = Map("infoMessage" -> Map("level" -> "danger", "message" -> ex.getMessage))
          renderUserPage(acc, context)
      }
    case ("logout", acc) =>
      removePorterCookie() {
        redirect(settings.endpointBaseUrl, StatusCodes.TemporaryRedirect)
      }

    case ("clearRememberedRealms", acc) =>
      val propname = rememberRealmProperty("").name
      val nacc = acc.updatedProps(_.filterKeys(k => !k.startsWith(propname)))
      porter ! UpdateAccount(settings.defaultRealm, nacc)
      val context = Map("infoMessage" -> Map("level" -> "success", "message" -> "Decision cache cleared."))
      renderUserPage(nacc, context)
  }

  private def renderLoginPage(params: Map[String, Any]) = complete {
    val page = settings.loginTemplate(defaultContext ++ params)
    HttpResponse(entity = HttpEntity(html, page))
  }

  private def renderUserPage(account: Account, params: Map[String, Any] = Map.empty) = complete {
    val accountMap = Map(
      "name" -> account.name.name,
      "props" -> account.props,
      "groups" -> account.groups.map(_.name).toList,
      "secrets" -> account.secrets.map(_.name.name)
    )
    val page = settings.userTemplate(defaultContext ++ Map("account" -> accountMap) ++ params ++ Map("changeSecretUrl" -> "/"))
    HttpResponse(entity = HttpEntity(html, page))
  }

  def updateSecret(realm: Ident, account: Account, pw: String): Directive1[Try[Account]] = {
    val newAccount = account.changeSecret(Password(settings.passwordCrypt)(pw))
    val f = (porter ? UpdateAccount(realm, newAccount)).mapTo[OperationFinished]
    onComplete(f).flatMap {
      case Success(OperationFinished(true, _)) => provide(Success(newAccount))
      case _ => provide(Failure(new Exception("Error setting secrets")))
    }
  }

  def updateSecretSubmit(realm: Ident, account: Account): Directive1[Try[Account]] =
    formField("porter.password1").flatMap { pw1 =>
      formField("porter.password2").flatMap { pw2 =>
        if (pw1 == pw2) updateSecret(realm, account, pw1)
        else provide(Failure(new Exception("Passwords are not equal")))
      }
    }

  def homeRoute: Route = {
    (path(PathMatchers.Slash) | path("")) {
      noCredentials {
        renderLoginPage(defaultContext)
      } ~
      credentials { creds =>
        authenticateAccount(creds, settings.defaultRealm)(timeout) { acc =>
          param("porter.action") { action =>
            actions.lift((action, acc)).getOrElse(reject())
          } ~
          setPorterCookieOnRememberme(acc) {
            renderUserPage(acc)
          }
        } ~
        renderLoginPage(Map("loginFailed" -> true))
      }
    }
  }
}
object HomeRoutes {
  val html = ContentType(MediaTypes.`text/html`)
}
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
          val context = Map("infoMessage" -> Map("level" -> "info", "message" -> "Secret changed."))
          setPorterCookie(nacc) {
            renderUserPage(Map("account" -> accountMap(nacc), "changeSecretUrl" -> "/") ++ context)
          }

        case Failure(ex) =>
          val context = Map("infoMessage" -> Map("level" -> "danger", "message" -> ex.getMessage))
          renderUserPage(Map("account" -> accountMap(acc), "changeSecretUrl" -> "/") ++ context)
      }
    case ("logout", acc) =>
      removePorterCookie() {
        renderLoginPage(defaultContext)
      }
  }

  private def renderLoginPage(params: Map[String, Any]) = complete {
    val page = settings.loginTemplate(defaultContext ++ params)
    HttpResponse(entity = HttpEntity(html, page))
  }

  private def renderUserPage(params: Map[String, Any]) = complete {
    val page = settings.userTemplate(defaultContext ++ params)
    HttpResponse(entity = HttpEntity(html, page))
  }

  private def accountMap(acc: Account): Map[String, Any] = Map(
    "name" -> acc.name.name,
    "props" -> acc.props,
    "groups" -> acc.groups.map(_.name).toList,
    "secrets" -> acc.secrets.map(_.name.name)
  )

  def updateSecret(realm: Ident, account: Account, pw: String): Directive1[Try[Account]] = {
    val newAccount = account.changeSecret(Password(pw))
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
          renderUserPage(Map("account" -> accountMap(acc), "changeSecretUrl" -> "/"))
        } ~
        renderLoginPage(Map("loginFailed" -> true))
      }
    }
  }
}
object HomeRoutes {
  val html = ContentType(MediaTypes.`text/html`)
}
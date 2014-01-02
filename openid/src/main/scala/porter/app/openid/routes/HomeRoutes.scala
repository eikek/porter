package porter.app.openid.routes

import spray.routing.{Directive1, PathMatchers, Directives, Route}
import spray.http._
import porter.model._
import scala.util.Try
import porter.app.akka.api.MutableStoreActor.messages.UpdateAccount
import scala.util.Failure
import spray.http.HttpResponse
import porter.model.Account
import scala.util.Success
import porter.app.akka.api.MutableStoreActor.messages.OperationFinished

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

    case ("updateProperties", acc) =>
      formFields { values =>
        val props = PropertyList.userProps.partition(p => values.get(p.name).exists(_.trim.nonEmpty)) match {
          case (add, remove) => add.map(p => p.setRaw(values(p.name))) ++ remove.map(p => p.remove)
        }
        if (props.isEmpty) {
          val context = Map("infoMessage" -> Map("level" -> "info", "message" -> "Nothing to save."))
          renderUserPage(acc, context)
        } else {
          val nacc = acc.updatedProps(props.reduce(_ andThen _))
          porter ! UpdateAccount(settings.defaultRealm, nacc)
          val context = Map("infoMessage" -> Map("level" -> "success", "message" -> "Account saved."))
          renderUserPage(nacc, context)
        }
      }
  }

  private def renderLoginPage(params: Map[String, Any]) = complete {
    val page = settings.loginTemplate(defaultContext ++ params)
    HttpResponse(entity = HttpEntity(html, page))
  }

  private def renderUserPage(account: Account, params: Map[String, Any] = Map.empty) = complete {
    import PropertyList._
    val accountMap = Map(
      "name" -> account.name.name,
      "props" -> account.props,
      "groups" -> account.groups.map(_.name).toList,
      "secrets" -> account.secrets.map(_.name.name)
    )
    val adminProps = List(lastLoginTime, successfulLogins, failedLogins).map { prop =>
      Map(
        "label" -> prop.name.substring("porter-admin-".length),
        "id" -> prop.name,
        "value" -> prop.getRaw(account.props).getOrElse("")
      )
    }
    val properties = userProps.map { prop =>
      Map(
        "label" -> prop.name.substring("porter-user-".length),
        "id" -> prop.name,
        "value" -> prop.getRaw(account.props).getOrElse("")
      )
    }
    val context = defaultContext ++ params ++
      Map("account" -> accountMap) ++
      Map("properties" -> properties) ++
      Map("adminProps" -> adminProps) ++
      Map("changeSecretUrl" -> "/")
    val page = settings.userTemplate(context)
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
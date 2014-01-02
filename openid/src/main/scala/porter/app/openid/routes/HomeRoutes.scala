package porter.app.openid.routes

import spray.routing.{Directive1, PathMatchers, Directives, Route}
import spray.http._
import porter.model._
import scala.util.Try
import scala.util.Failure
import spray.http.HttpResponse
import porter.model.Account
import scala.util.Success

trait HomeRoutes {
  self: OpenIdDirectives with PageDirectives with AuthDirectives =>

  import Directives._
  import HomeRoutes._
  import PageDirectives._
  import akka.pattern.ask
  import _root_.porter.app.akka.Porter.Messages.mutableStore._

  type Action = PartialFunction[(String, Account), Route]

  private lazy val actions: Action = {
    case ("changeSecret", acc) =>
      formField("porter.currentpassword") { cpw =>
        authenticateAccount(Set(PasswordCredentials(acc.name, cpw)), settings.defaultRealm)(timeout) { _ =>
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
        } ~
        renderUserPage(acc, Map("infoMessage" -> Map("level" -> "danger", "message" -> "Authentication failed.")))
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
    case ("removeAccount", acc) =>
      paramOpt("porter.removeAccount") { p =>
        if (p == Some("on")) {
          log.info(s"About to delete account '${acc.name.name}'.")
          val f = (porter ? DeleteAccount(settings.defaultRealm, acc.name)).mapTo[OperationFinished]
          onComplete(f) {
            case Success(of) if of.result =>
              log.info(s"Account '${acc.name.name}' deleted.")
              removePorterCookie() {
                redirect(settings.endpointBaseUrl, StatusCodes.TemporaryRedirect)
              }
            case Success(of) =>
              log.error(s"Failed to delete account '${acc.name.name}")
              val context = Map("infoMessage" -> Map("level" -> "danger", "message" -> "Could not remove account."))
              renderUserPage(acc, context)
            case Failure(ex) =>
              log.error(ex, s"Failed to delete account '${acc.name.name}")
              val context = Map("infoMessage" -> Map("level" -> "danger", "message" -> s"Could not remove account: ${ex.getMessage}."))
              renderUserPage(acc, context)
          }
        } else {
          renderUserPage(acc, Map("infoMessage" -> Map("level" -> "info", "message" -> "Please enable the check box to allow deletion.")))
        }
      }
  }

  private def renderLoginPage(params: Map[String, Any]) = removePorterCookie() {
    complete {
      val page = settings.loginTemplate(defaultContext ++ params)
      HttpResponse(entity = HttpEntity(html, page))
    }
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
      Map("actionUrl" -> settings.endpointBaseUrl.path.toString())
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
    path(PathMatchers.separateOnSlashes(settings.endpointBaseUrl.path.toString())) {
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
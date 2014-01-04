package porter.app.openid.routes

import spray.routing.{Directive1, PathMatchers, Directives, Route}
import spray.http._
import porter.model._
import scala.util.Try
import scala.util.Failure
import spray.http.HttpResponse
import porter.model.Account
import scala.util.Success
import porter.app.client.spray.{PorterDirectives, PorterContext, PorterAuthenticator}
import spray.routing.authentication.UserPass

trait HomeRoutes extends OpenIdDirectives with PageDirectives with AuthDirectives {
  self: OpenIdActors =>

  import Directives._
  import PageDirectives._
  import akka.pattern.ask
  import porter.app.akka.Porter.Messages.mutableStore._

  type Action = PartialFunction[(String, Account), Route]

  private val porterContext = PorterContext(porterRef, settings.defaultRealm, settings.decider)
  private def PorterAuth = PorterAuthenticator(porterContext, settings.cookieKey, settings.cookieName)

  private def message(level: String = "info", msg: String) = Map("infoMessage" -> Map(
    "level" -> level, "message" -> msg
  ))

  private lazy val actions: Action = {
    case ("changeSecret", acc) =>
      formField("porter.currentpassword") { cpw =>
        authcAccount(settings.defaultRealm, Set(PasswordCredentials(acc.name, cpw)))(timeout) { _ =>
          updateSecretSubmit(settings.defaultRealm, acc) {
            case Success(nacc) =>
              setPorterCookieOnRememberme(nacc) {
                renderUserPage(nacc, message("success", "Secret changed."))
              }
            case Failure(ex) =>
              renderUserPage(acc, message("danger", ex.getMessage))
          }
        } ~
        renderUserPage(acc, message("danger", "Authentication failed."))
      }

    case ("logout", acc) =>
      removePorterCookie() {
        redirect(settings.endpointBaseUrl, StatusCodes.TemporaryRedirect)
      }

    case ("clearRememberedRealms", acc) =>
      val propname = rememberRealmPropName
      val nacc = acc.updatedProps(_.filterKeys(k => !k.startsWith(propname)))
      porterRef ! UpdateAccount(settings.defaultRealm, nacc)
      renderUserPage(nacc, message("success", "Decision cache cleared."))

    case ("updateProperties", acc) =>
      formFields { values =>
        val props = PropertyList.userProps.partition(p => values.get(p.name).exists(_.trim.nonEmpty)) match {
          case (add, remove) => add.map(p => p.setRaw(values(p.name))) ++ remove.map(p => p.remove)
        }
        if (props.isEmpty) {
          renderUserPage(acc, message("info", "Nothing to save."))
        } else {
          val nacc = acc.updatedProps(props.reduce(_ andThen _))
          porterRef ! UpdateAccount(settings.defaultRealm, nacc)
          renderUserPage(nacc, message("success", "Account saved."))
        }
      }
    case ("removeAccount", acc) =>
      paramOpt("porter.removeAccount") { p =>
        if (p == Some("on")) {
          log.info(s"About to delete account '${acc.name.name}'.")
          val f = (porterRef ? DeleteAccount(settings.defaultRealm, acc.name)).mapTo[OperationFinished]
          onComplete(f) {
            case Success(of) if of.result =>
              log.info(s"Account '${acc.name.name}' deleted.")
              removePorterCookie() {
                redirect(settings.endpointBaseUrl, StatusCodes.TemporaryRedirect)
              }
            case Success(of) =>
              log.error(s"Failed to delete account '${acc.name.name}")
              renderUserPage(acc, message("info", "Could not remove account."))
            case Failure(ex) =>
              log.error(ex, s"Failed to delete account '${acc.name.name}")
              renderUserPage(acc, message("danger", "Could not remove account: " +ex.getMessage))
          }
        } else {
          renderUserPage(acc, message("info", "Please enable the check box to allow deletion."))
        }
      }
  }

  private def renderUserPage(account: Account, params: Map[String, Any] = Map.empty) = complete {
    import PropertyList._
    import Implicits._
    val adminProps = List(lastLoginTime, successfulLogins, failedLogins)
    val context = defaultContext ++ params ++
      Map("account" -> account.toMap) ++
      Map("properties" -> userProps.map(_.toMap(account, _.substring("porter-user-".length)))) ++
      Map("adminProps" -> adminProps.map(_.toMap(account, _.substring("porter-admin-".length)))) ++
      Map("actionUrl" -> settings.endpointBaseUrl.path.toString())
    val page = settings.userTemplate(context)
    HttpResponse(entity = HttpEntity(html, page))
  }

  def updateSecret(realm: Ident, account: Account, pw: String): Directive1[Try[Account]] = {
    val newAccount = account.changeSecret(Password(settings.passwordCrypt)(pw))
    val f = (porterRef ? UpdateAccount(realm, newAccount)).mapTo[OperationFinished]
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
        renderLoginPage(settings.endpointBaseUrl.path.toString(), failed = false)
      } ~
      authenticate(PorterAuth) { acc =>
        param("porter.action") { action =>
          actions.lift((action, acc)).getOrElse(reject())
        } ~
        setPorterCookieOnRememberme(acc) {
          renderUserPage(acc)
        }
      } ~
      renderLoginPage(settings.endpointBaseUrl.path.toString(), failed = true)
    }
  }
}
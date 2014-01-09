package porter.app.openid.routes

import spray.routing._
import spray.http._
import porter.model._
import porter.app.client.spray.PorterAuthenticator
import porter.app.openid.routes.ManageRoutes.Registration
import scala.util.{Try, Failure, Success}
import scala.Some
import spray.http.HttpResponse
import porter.model.Account
import porter.app.client.spray.PorterContext
import porter.app.openid.OpenIdServiceSettings
import porter.app.akka.api.StoreActor.messages.{FindAccountsResp, FindAccounts}
import porter.app.akka.PorterUtil
import java.util.Locale

trait ManageRoutes extends OpenIdDirectives with PageDirectives with AuthDirectives {
  self: OpenIdActors =>

  import Directives._
  import PageDirectives._
  import akka.pattern.ask
  import porter.app.akka.Porter.Messages.mutableStore._

  type Action = PartialFunction[(String, Account), Route]

  private val porterContext = PorterContext(porterRef, settings.defaultRealm, settings.decider)
  private def PorterAuth = PorterAuthenticator(porterContext, settings.cookieKey, settings.cookieName)

  private def changePwFuture(cpw: ChangePassword) =
    PorterUtil.changePassword(porterRef, cpw.realm, cpw.current, cpw.plain, settings.passwordCrypt, settings.decider)

  private def message(level: String = "info", msg: String) = Map("infoMessage" -> Map(
    "level" -> level, "message" -> msg
  ))

  private lazy val actions: Action = {
    case ("changeSecret", acc) =>
      changePassword(acc.name) { cpw =>
        onComplete(changePwFuture(cpw)) {
          case Success(nacc) =>
            setPorterCookieOnRememberme(nacc) {
              renderUserPage(nacc, message("success", "Secret changed."))
            }
          case Failure(ex) =>
            renderUserPage(acc, message("danger", ex.getMessage))
        }
      }

    case ("logout", acc) =>
      removePorterCookie() {
        redirect(settings.endpointBaseUrl.toRelative, StatusCodes.TemporaryRedirect)
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
                redirect(settings.endpointBaseUrl.toRelative, StatusCodes.Found)
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

  private def renderUserPage(account: Account, params: Map[String, Any] = Map.empty) =
    localeWithDefault(Some(account)) { loc =>
      complete {
        println("Locale: "+ loc)
        import PropertyList._
        import Implicits._
        val adminProps = List(lastLoginTime, successfulLogins, failedLogins)
        val context = defaultContext ++ params ++
          Map("account" -> account.toMap) ++
          Map("properties" -> userProps.map(_.toMap(account, loc, _.substring("porter-user-".length)))) ++
          Map("adminProps" -> adminProps.map(_.toMap(account, loc, _.substring("porter-admin-".length)))) ++
          Map("actionUrl" -> settings.endpointBaseUrl.toRelative.toString())
        val page = settings.userTemplate(context)
        HttpResponse(entity = HttpEntity(html, page))
      }
    }

  private def renderRegistrationPage(fields: Map[String, String] = Map.empty, failed: List[String] = Nil) = {
    if (settings.registrationEnabled) {
      val context = defaultContext ++ Map(
        "actionUrl" -> settings.endpointBaseUrl.toRelative.toString(),
        "loginUrl" -> settings.endpointBaseUrl.toRelative.toString(),
        "withEmail" -> settings.registrationRequiresEmail,
        "withKey" -> settings.registrationKey.getOrElse(""),
        "registerFailed" -> failed.nonEmpty,
        "registerFailedReasons" -> failed,
        "fields" -> fields
      )
      val page = settings.registerTemplate(context)
      complete(HttpResponse(entity = HttpEntity(html, page)))
    } else {
      renderLoginPage(settings.endpointBaseUrl, failed = false)
    }
  }

  def changePassword(account: Ident): Directive1[ChangePassword] =
    formField("porter.currentpassword").flatMap { cpw =>
      formField("porter.password1").flatMap { pw1 =>
        formField("porter.password2").flatMap { pw2 =>
          if (pw1 == pw2) provide(ChangePassword(settings.defaultRealm, PasswordCredentials(account, cpw), pw1))
          else reject()
        }
      }
    }


  private def registration: Directive1[Registration] = formFields.flatMap {
    case Registration(reg) => provide(reg)
    case _ => reject()
  }

  private def invalidRegistration(reg: Registration): Directive1[List[String]] = {
    val errors = reg.isValid(settings)
    if (errors.nonEmpty) provide(errors) else {
      val f = for {
        resp <- (porterRef ? FindAccounts(settings.defaultRealm, Set(reg.name))).mapTo[FindAccountsResp]
      } yield resp.accounts.isEmpty
      onComplete(f).flatMap {
        case Success(result) if result => reject()
        case _ => provide(List("Account name already in use."))
      }
    }
  }
  
  private def createAccount(reg: Registration): Directive1[Account] =
    onComplete(PorterUtil.createNewAccount(porterRef, settings.defaultRealm, reg.toAccount(settings))).flatMap {
      case Success(account) => provide(account)
      case Failure(ex) => log.error(ex, "Unable to create account!"); reject()
    }

  def homeRoute: Route = {
    path(PathMatchers.separateOnSlashes(settings.endpointBaseUrl.path.toString())) {
      paramIs("porter.action", "register") {
        registration { reg =>
          invalidRegistration(reg) { errors =>
            renderRegistrationPage(reg.fields, errors)
          } ~
          createAccount(reg) { acc =>
            setPorterCookieOnRememberme(acc) {
              redirect(settings.endpointBaseUrl.toRelative, StatusCodes.Found)
            }
          } ~ renderErrorPage
        } ~ renderRegistrationPage(Map.empty, List("Invalid registration request."))
      } ~
      parameter("register") { _ =>
        if (settings.registrationEnabled) renderRegistrationPage()
        else reject()
      } ~
      noCredentials {
        renderLoginPage(settings.endpointBaseUrl, failed = false)
      } ~
      authenticate(PorterAuth) { acc =>
        param("porter.action") { action =>
          actions.lift((action, acc)).getOrElse(reject())
        } ~
        setPorterCookieOnRememberme(acc) {
          renderUserPage(acc)
        }
      } ~
      renderLoginPage(settings.endpointBaseUrl, failed = true)
    }
  }
}

object ManageRoutes {

  case class Registration(name: String,
                          email: Option[String],
                          password1: String,
                          password2: String,
                          key: Option[String],
                          fields: Map[String, String]) {

    def isValid(settings: OpenIdServiceSettings): List[String] = {
      if (!settings.registrationEnabled) List("Registration disabled")
      else {
        List(
          if (Ident.fromString(name).isDefined) "" else "Account name is invalid.",
          if (password1.trim.nonEmpty && password1 == password2) "" else "Password is required and both must match.",
          if (settings.registrationKey == key) "" else "Registration key is invalid.",
          if (!settings.registrationRequiresEmail || email.exists(_.trim.nonEmpty)) "" else "Email is required."
        ).filter(_.nonEmpty)
      }
    }

    def toAccount(settings: OpenIdServiceSettings): Account = {
      val a = Account(name = name, secrets = Seq(Password(settings.passwordCrypt)(password1)))
      email.map(PropertyList.email.set).map(a.updatedProps).getOrElse(a)
    }
  }

  object Registration {
    //Todo: look at how marshallers work in spray
    def unapply(data: Map[String, String]): Option[Registration] = {
      val names = List("porter.username", "porter.email", "porter.password1", "porter.password2", "porter.registerKey")
      val values = names.map(k => data.get(k))
      values match {
        case Some(name) :: email :: Some(pw1) :: Some(pw2) :: key :: Nil =>
          Some(Registration(name, email, pw1, pw2, key, data.filterNot(_._1.startsWith("porter.password"))))
        case _ => None
      }
    }
  }
}
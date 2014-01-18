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
import porter.app.openid.common.MustacheContext
import porter.model.Property.{BinaryValue, BinaryProperty}

trait ManageRoutes extends OpenIdDirectives with PageDirectives with AuthDirectives {
  self: OpenIdActors =>

  import Directives._
  import PageDirectives._
  import akka.pattern.ask
  import porter.app.akka.Porter.Messages.mutableStore._

  type Action = PartialFunction[(String, Account), Route]

  private val porterContext = PorterContext(porterRef, settings.defaultRealm, settings.decider)
  private def PorterAuth = PorterAuthenticator(porterContext, settings.cookieKey, settings.cookieName)
  private def avatarUrl(account: Account) = settings.openIdUrl.withPath(settings.openIdUrl.path / "avatar" / account.name.name).withQuery("size" -> "80").toRelative

  private def changePwFuture(cpw: ChangePassword) =
    PorterUtil.changePassword(porterRef, cpw.realm, cpw.current, cpw.plain, settings.passwordCrypt, settings.decider)

  private def message(level: String = "info", msg: String) = Map("infoMessage" -> Map(
    "level" -> level, "message" -> msg
  ))
  private def messageItems(level: String = "info", msg: String, items: List[String]) = Map("infoMessage" -> Map(
    "level" -> level, "message" -> msg, "messageItems" -> items
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
      entity(as[MultipartFormData]) { data =>
        updateProperties(data) match {
          case Right(fun) =>
            val nacc = acc.updatedProps(fun)
            val upd = UpdateAccount(settings.defaultRealm, nacc)
            onComplete((porterRef ? upd).mapTo[OperationFinished]) {
              case Success(OperationFinished(true)) => renderUserPage(nacc, message("success", "Account saved."))
              case _ => renderUserPage(acc, message("danger", "Error saving account."))
            }
          case Left(msgs) =>
            renderUserPage(acc, messageItems("danger", "Invalid property values.", msgs))
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
    case ("deleteAvatar", acc) =>
      val nacc = acc.updatedProps(PropertyList.avatar.remove)
      val upd = UpdateAccount(settings.defaultRealm, nacc)
      onComplete((porterRef ? upd).mapTo[OperationFinished]) {
        case Success(OperationFinished(true)) => renderUserPage(nacc, message("success", "Account saved."))
        case _ => renderUserPage(acc, message("danger", "Error saving account."))
      }
  }

  private def validateProperties(props: Seq[Property[_]], data: MultipartFormData): List[String] = {
    val birthdate = """(\d{4}\-\d{2}\-\d{2})""".r
    props.map {
      case PropertyList.avatar =>
        val len = data.get(PropertyList.avatar.name).get.entity.data.length
        if (len > settings.avatarMaxUploadSize * 1024) s"Image is too large, only ${settings.avatarMaxUploadSize}kb are allowed."
        else ""
      case PropertyList.birthday =>
        val in = data.get(PropertyList.birthday.name).get.entity.asString
        in match {
          case birthdate(_) => ""
          case _ => s"Invalid birthdate string '$in'. Use pattern 'yyyy-mm-dd'"
        }
      case PropertyList.email =>
        val in = data.get(PropertyList.email.name).get.entity.asString
        if (in.indexOf('@') < 0) s"'$in' does not seem to be a valid email"
        else ""
      case _ => ""
    }.filter(_.nonEmpty).toList
  }

  private def updateProperties(data: MultipartFormData): Either[List[String], Properties => Properties] = {
    val (add, remove) = PropertyList.userProps.partition(p => data.get(p.name).exists(_.entity.nonEmpty))
    validateProperties(add, data) match {
      case Nil =>
        val fadds = add.map {
          case PropertyList.avatar =>
            data.get(PropertyList.avatar.name) match {
              case Some(bp@BodyPart(entity, header)) =>
                val ct = header.collect{ case HttpHeaders.`Content-Type`(c) =>  c.toString() }
                  .headOption getOrElse "image/jpg"
                PropertyList.avatar.set(BinaryValue(ct, entity.data.toByteArray))
              case _ => identity[Properties]_
            }
          case prop =>
            data.get(prop.name).map(bp => prop.setRaw(bp.entity.asString)).getOrElse(identity[Properties]_)
        }
        val fremoves = remove.filterNot(_ == PropertyList.avatar).map(p => p.remove)
        Right((fadds ++ fremoves).reduce(_ andThen _))

      case msgs => Left(msgs)
    }
  }

  private def renderUserPage(account: Account, params: Map[String, Any] = Map.empty) = {
    def createPage(loc: Locale) = {
      import MustacheContext._
      import Templating._
      import PropertyList._
      val adminFields = List(lastLoginTime, successfulLogins, failedLogins)
        .map(p => PropertyField(p, p.name.substring(13), account, loc))
      val userFields = userProps.map(p => PropertyField(p, p.name.substring(12), account, loc)).toList
      val data = Data.appendRaw(params)
        .andThen(KeyName.properties.put(userFields))
        .andThen(KeyName.adminProps.put(adminFields))
        .andThen(KeyName.account.put(account))
        .andThen(KeyName.actionUrl.put(settings.endpointBaseUrl.toRelative))
        .andThen(KeyName.avatarUrl.put(avatarUrl(account)))

      settings.userTemplate(data(buildInfoMap))
    }

    localeWithDefault(Some(account)) { loc =>
      complete {
        HttpResponse(entity = HttpEntity(html, createPage(Locale.getDefault)))
      }
    }
  }

  private def renderRegistrationPage(fields: Map[String, String] = Map.empty, failed: List[String] = Nil) = {
    import MustacheContext._
    import Templating._
    if (settings.registrationEnabled) {
      val data = KeyName.actionUrl.put(settings.endpointBaseUrl.toRelative)
        .andThen(KeyName.loginUrl.put(settings.endpointBaseUrl.toRelative))
        .andThen(KeyName.withEmail.put(settings.registrationRequiresEmail))
        .andThen(KeyName.withKey.put(settings.registrationKey))
        .andThen(KeyName.registerFailed.put(failed.nonEmpty))
        .andThen(KeyName.registerFailedReasons.put(failed))
        .andThen(KeyName.fields.putRaw(fields))
      val page = settings.registerTemplate(data(buildInfoMap))
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
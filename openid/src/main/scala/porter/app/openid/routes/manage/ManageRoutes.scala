package porter.app.openid.routes.manage

import spray.routing._
import spray.http._
import porter.model._
import porter.app.client.spray.PorterAuthenticator
import java.util.Locale
import porter.app.openid.common.MustacheContext
import porter.app.openid.routes._
import scala.Some
import spray.http.HttpResponse
import porter.model.Account
import porter.app.client.spray.PorterContext

trait ManageRoutes extends OpenIdDirectives with PageDirectives with AuthDirectives
  with Registration
  with ChangeSecret
  with ClearRememberedRealms
  with DeleteAvatar
  with LogoutAction
  with RemoveAccount
  with UpdateProperties { self: OpenIdActors =>

  import Directives._
  import PageDirectives._

  private val porterContext = PorterContext(porterRef, settings.defaultRealm, settings.decider)
  private def PorterAuth = PorterAuthenticator(porterContext, settings.cookieKey, settings.cookieName)
  private def avatarUrl(account: Account) = settings.openIdUrl.withPath(settings.openIdUrl.path / "avatar" / account.name.name).withQuery("size" -> "80").toRelative

  private val submissions = List(changeSecret, clearReamls, changePropertiesSubmission,
    logout, deleteAccount, deleteAvatar).reduce(_ orElse _)

  def homeRoute: Route = {
    path(PathMatchers.separateOnSlashes(settings.endpointBaseUrl.path.toString())) {
      paramIs("porter.action", "register") {
        registrationRoute
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
          submissions.lift(Action(action, porterContext, acc)).getOrElse(reject())
        } ~
        setPorterCookieOnRememberme(acc) {
          renderUserPage(acc)
        }
      } ~
      renderLoginPage(settings.endpointBaseUrl, failed = true)
    }
  }

  def redirectToUserPage =
    redirect(settings.endpointBaseUrl.toRelative, StatusCodes.Found)

  def renderUserPage(account: Account, msg: Message = Message.empty) = {
    def createPage(loc: Locale) = {
      import MustacheContext._
      import Templating._
      import PropertyList._
      val adminFields = List(lastLoginTime, successfulLogins, failedLogins)
        .map(p => PropertyField(p, p.name.substring(13), account, loc))
      val userFields = userProps.map(p => PropertyField(p, p.name.substring(12), account, loc)).toList
      val data = KeyName.infoMessage.put(msg)
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

  def renderRegistrationPage(fields: Map[String, String] = Map.empty, failed: List[String] = Nil) = {
    import MustacheContext._
    import Templating._
    val data = KeyName.actionUrl.put(settings.endpointBaseUrl.toRelative)
      .andThen(KeyName.loginUrl.put(settings.endpointBaseUrl.toRelative))
      .andThen(KeyName.withEmail.put(settings.registrationRequiresEmail))
      .andThen(KeyName.withKey.put(settings.registrationKey))
      .andThen(KeyName.registerFailed.put(failed.nonEmpty))
      .andThen(KeyName.registerFailedReasons.put(failed))
      .andThen(KeyName.fields.putRaw(fields))
    val page = settings.registerTemplate(data(buildInfoMap))
    complete(HttpResponse(entity = HttpEntity(html, page)))
  }
}

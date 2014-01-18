package porter.app.openid.routes.manage

import porter.app.openid.routes.OpenIdActors
import spray.routing.Directives._
import porter.model.{PasswordCredentials, Ident}
import spray.routing._
import scala.util.Success
import scala.util.Failure
import porter.client.Messages.mutableStore.ChangePassword
import porter.app.akka.PorterUtil

trait ChangeSecret {
  self: ManageRoutes with OpenIdActors =>

  def changeSecret: Submission = {
    case Action("changeSecret", ctx, acc) =>
      changePassword(acc.name) { cpw =>
        onComplete(changePwFuture(cpw)) {
          case Success(nacc) =>
            setPorterCookieOnRememberme(nacc) {
              renderUserPage(nacc, Message.success("Secret changed."))
            }
          case Failure(ex) =>
            renderUserPage(acc, Message.error(ex.getMessage))
        }
      }
  }

  private def changePassword(account: Ident): Directive1[ChangePassword] =
    formField("porter.currentpassword").flatMap { cpw =>
      formField("porter.password1").flatMap { pw1 =>
        formField("porter.password2").flatMap { pw2 =>
          if (pw1 == pw2) provide(ChangePassword(settings.defaultRealm, PasswordCredentials(account, cpw), pw1))
          else reject()
        }
      }
    }

  private def changePwFuture(cpw: ChangePassword) =
    PorterUtil.changePassword(porterRef, cpw.realm, cpw.current, cpw.plain, settings.passwordCrypt, settings.decider)

}

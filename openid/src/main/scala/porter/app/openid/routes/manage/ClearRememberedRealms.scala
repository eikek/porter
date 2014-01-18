package porter.app.openid.routes.manage

import porter.client.Messages.mutableStore.UpdateAccount
import porter.app.openid.routes.OpenIdActors

trait ClearRememberedRealms {
  self: ManageRoutes with OpenIdActors =>

  def clearReamls: Submission = {
    case Action("clearRememberedRealms", ctx, acc) =>
      val propname = rememberRealmPropName
      val nacc = acc.updatedProps(_.filterKeys(k => !k.startsWith(propname)))
      ctx.porterRef ! UpdateAccount(settings.defaultRealm, nacc)
      renderUserPage(nacc, Message.success("Decision cache cleared."))
  }
}

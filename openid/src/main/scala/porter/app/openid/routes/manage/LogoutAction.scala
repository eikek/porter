package porter.app.openid.routes.manage

import porter.app.openid.routes.OpenIdActors

trait LogoutAction {
  self: ManageRoutes with OpenIdActors =>

  def logout: Submission = {
    case Action("logout", ctx, acc) =>
      removePorterCookie() {
        redirectToUserPage
      }
  }
}

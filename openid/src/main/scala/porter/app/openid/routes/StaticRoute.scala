package porter.app.openid.routes

import spray.routing.{PathMatchers, Route}
import spray.http.Uri

trait StaticRoute {
  self: OpenIdActors =>

  import spray.routing.Directives._
  import spray.routing.RoutingSettings.default

  private def staticPath =
    PathMatchers.separateOnSlashes(settings.openIdUrl.path.dropChars(1).toString()) / "statics"

  def staticRoute: Route = {
    path(staticPath / RestPath) { rest =>
      val relative = if (rest.startsWithSlash) rest.dropChars(1) else rest
      val resource = settings.staticResourceDir.resolve(relative.toString())
      getFromFile(resource.toFile) ~ getFromResource("porter/app/openid/assets/"+relative.toString())
    }
  }
}

/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

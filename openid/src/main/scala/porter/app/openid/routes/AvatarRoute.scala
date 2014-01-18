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

import spray.routing.{Directive0, PathMatchers, Directives, Route}
import porter.app.client.spray.{PorterContext, PorterDirectives}
import porter.model.{PropertyList, Account, Ident}
import spray.http.{HttpData, MediaType, HttpEntity, ContentType}
import porter.app.openid.common.Harmonicon
import porter.util.Hash
import javax.imageio.ImageIO
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import porter.app.openid.common.Harmonicon.{ImageSettings, Timespan}
import java.awt.image.BufferedImage
import java.awt.{RenderingHints, Graphics2D, Transparency}
import porter.app.openid.AvatarActor.{AvatarImageResp, GetAvatarImage}

trait AvatarRoute extends Directives with PorterDirectives {
  self: OpenIdActors =>

  import akka.pattern.ask

  private val avatarPath = PathMatchers.separateOnSlashes(settings.openIdUrl.toRelative.path.dropChars(1).toString()) / "avatar"

  private def contentType(s: String) = ContentType(MediaType.custom(s))

  private def imageSize = parameter("size".as[Int]) | provide(125)

  private def respondWithLastModified(time: Option[Long]): Directive0 =
    time.map(respondWithLastModifiedHeader).getOrElse(pass)

  def avatarRoute: Route = {
    (get & path(avatarPath / Segment)) { ident =>
      imageSize { size =>
        val f = (avatarRef ? GetAvatarImage(settings.defaultRealm, ident, size)).mapTo[AvatarImageResp]
        onSuccess(f) { resp =>
          respondWithLastModified(resp.lastModified) {
            complete(HttpEntity(contentType(resp.contentType), HttpData(resp.data)))
          }
        }
      }
    }
  }
}

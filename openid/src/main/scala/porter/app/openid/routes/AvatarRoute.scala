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

package porter.app.openid

import com.typesafe.config.Config
import scala.util.Try
import java.nio.file.{Files, Paths}
import akka.actor.{ExtendedActorSystem, ExtensionIdProvider, ExtensionId, Extension}
import porter.util.{AES, Base64}
import spray.http.Uri
import porter.model.Ident

class OpenIdSettings(cfg: Config) extends Extension with OpenIdServiceSettings {
  import collection.JavaConverters._

  val bindingHost = Try(cfg.getString("host")).getOrElse("localhost")
  val bindinPort = Try(cfg.getInt("port")).getOrElse(8888)

  val staticResourceDir = Paths.get(cfg.getString("static-resource-dir"))
  val templateDir = Paths.get(cfg.getString("template-dir"))

  val cookieName = cfg.getString("cookie-name")
  val cookieKey = Try(cfg.getString("cookie-key")).map(Base64.decode).getOrElse(AES.generateRandomKey).toVector

  val realms = cfg.getStringList("realms").asScala.toList.map(Ident.apply)

  val defaultRealm = Ident(Try(cfg.getString("default-realm")).getOrElse("default"))

  val endpointBaseUrl = Try(cfg.getString("endpoint-base-url")).map(Uri.apply).getOrElse(Uri(s"http://$bindingHost:$bindinPort"))

}

object OpenIdSettings extends ExtensionId[OpenIdSettings] with ExtensionIdProvider {
  def lookup() = OpenIdSettings

  def createExtension(system: ExtendedActorSystem) =
    new OpenIdSettings(system.settings.config.getConfig("porter.openid"))
}
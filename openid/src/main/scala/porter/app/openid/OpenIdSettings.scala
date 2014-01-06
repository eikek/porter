package porter.app.openid

import com.typesafe.config.Config
import scala.util.Try
import java.nio.file.Paths
import akka.actor._
import porter.util.{AES, Base64}
import spray.http.Uri
import porter.model.{PasswordCrypt, Ident}

class OpenIdSettings(cfg: Config, da: DynamicAccess) extends Extension with OpenIdServiceSettings {
  import collection.JavaConverters._

  val bindingHost = Try(cfg.getString("host")).getOrElse("localhost")
  val bindinPort = Try(cfg.getInt("port")).getOrElse(8888)

  val passwordCrypt = {
    val config = cfg.getString("password-crypt")
    PasswordCrypt(config).getOrElse(sys.error(s"Invalid configuration for password-crypt: $config"))
  }

  val decider = da.getObjectFor[porter.auth.Decider](cfg.getString("decider")).get

  val contact = Try(cfg.getString("contact")).toOption.filter(_.nonEmpty)

  val staticResourceDir = Paths.get(cfg.getString("static-resource-dir"))
  val templateDir = Paths.get(cfg.getString("template-dir"))

  val cookieName = cfg.getString("cookie-name")
  val cookieKey = Try(cfg.getString("cookie-key")).map(Base64.decode).getOrElse(AES.generateRandomKey).toVector
  val cookieSecure = cfg.getBoolean("cookie-secure")

  val realms = cfg.getStringList("realms").asScala.toList.map(Ident.apply)

  val defaultRealm = Ident(Try(cfg.getString("default-realm")).getOrElse("default"))

  val endpointBaseUrl = Try(cfg.getString("endpoint-base-url")).map(Uri.apply).getOrElse(Uri(s"http://$bindingHost:$bindinPort"))

  val registrationEnabled = cfg.getBoolean("registration-enabled")
  val registrationRequiresEmail = cfg.getBoolean("registration-requires-email")
  val registrationKey = Try(cfg.getString("registration-invitation-key")).toOption.filter(_.trim.nonEmpty)
}

object OpenIdSettings extends ExtensionId[OpenIdSettings] with ExtensionIdProvider {
  def lookup() = OpenIdSettings

  def createExtension(system: ExtendedActorSystem) =
    new OpenIdSettings(system.settings.config.getConfig("porter.openid"), system.dynamicAccess)
}
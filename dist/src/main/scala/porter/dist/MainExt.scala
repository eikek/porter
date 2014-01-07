package porter.dist

import akka.actor._
import porter.dist.MainExt.{HttpSettings, InterfaceSettings}
import porter.app.openid.OpenIdSettings
import porter.auth.Decider
import porter.model.PasswordCrypt

object MainExt extends ExtensionId[MainExt] with ExtensionIdProvider {
  def lookup() = MainExt

  def createExtension(system: ExtendedActorSystem) =
    MainExt(system.provider.getDefaultAddress, system)

  case class InterfaceSettings(host: String, port: Int, enabled: Boolean)
  case class HttpSettings(iface: InterfaceSettings, decider: Decider, crypt: PasswordCrypt)
}

case class MainExt(address: Address, private val system: ExtendedActorSystem) extends Extension {

  def pathFor(porter: ActorRef) = ActorPath.fromString(
    address.toString + porter.path.elements.mkString("/", "/", ""))

  private val config = system.settings.config.getConfig("porter")
  val openidSettings = new OpenIdSettings(config.getConfig("openid"), system.dynamicAccess)

  val telnet = InterfaceSettings(
    config.getString("telnet.host"),
    config.getInt("telnet.port"),
    config.getBoolean("telnet.enabled")
  )

  val http = HttpSettings(
    InterfaceSettings(
      config.getString("http.host"),
      config.getInt("http.port"),
      config.getBoolean("http.enabled")
    ),
    system.dynamicAccess.getObjectFor[porter.auth.Decider](config.getString("http.decider")).get,
    PasswordCrypt(config.getString("http.password-crypt"))
      .getOrElse(sys.error(s"Invalid configuration for password-crypt '${config.getString("http.password-crypt")}'"))
  )

  val openid = InterfaceSettings(
    config.getString("openid.host"),
    config.getInt("openid.port"),
    config.getBoolean("openid.enabled")
  )
}

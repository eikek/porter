package porter.dist

import akka.actor._
import porter.dist.MainExt.InterfaceSettings

object MainExt extends ExtensionId[MainExt] with ExtensionIdProvider {
  def lookup() = MainExt

  def createExtension(system: ExtendedActorSystem) =
    MainExt(system.provider.getDefaultAddress, system)

  case class InterfaceSettings(host: String, port: Int, enabled: Boolean)
}

case class MainExt(address: Address, private val system: ExtendedActorSystem) extends Extension {

  def pathFor(porter: ActorRef) = ActorPath.fromString(
    address.toString + porter.path.elements.mkString("/", "/", ""))

  private val config = system.settings.config.getConfig("porter")

  val telnet = InterfaceSettings(
    config.getString("telnet.host"),
    config.getInt("telnet.port"),
    config.getBoolean("telnet.enabled")
  )

  val http = InterfaceSettings(
    config.getString("http.host"),
    config.getInt("http.port"),
    config.getBoolean("http.enabled")
  )
}

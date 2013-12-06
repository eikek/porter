package porter.app.akka.api

import akka.actor._
import porter.app.PorterSettings

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 05.12.13 00:56
 */
object Porter extends ExtensionId[Porter] with ExtensionIdProvider {
  def lookup() = Porter

  def createExtension(system: ExtendedActorSystem) = {
    val config = system.settings.config
    val settings = new PorterSettings(config.getConfig("porter"), system.dynamicAccess)
    Porter(settings, system.provider.getDefaultAddress, system)
  }

}

case class Porter(settings: PorterSettings, address: Address, private val factory: ActorRefFactory) extends Extension {

  lazy val singlePorter = factory.actorOf(porterProps)

  val porterProps = Props(classOf[PorterMain], settings)

  def porterPath(porter: ActorRef) = ActorPath.fromString(
    address.toString + porter.path.elements.mkString("/", "/", ""))
}

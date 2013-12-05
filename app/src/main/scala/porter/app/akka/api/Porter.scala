package porter.app.akka.api

import akka.actor.{ExtendedActorSystem, Extension, ExtensionIdProvider, ExtensionId}
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
    Porter(settings)
  }

}

case class Porter(settings: PorterSettings) extends Extension

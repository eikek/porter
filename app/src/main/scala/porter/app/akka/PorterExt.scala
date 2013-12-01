package porter.app.akka

import akka.actor._
import porter.app.PorterSettings
import porter.auth.{RuleFactory, Porter}
import porter.store.MultiStore
import porter.model.Ident

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 19:52
 */
class PorterExt(val ref: ActorRef, val settings: PorterSettings, address: Address) extends Extension {

  def porterPath = ActorPath.fromString(address.toString + ref.path.elements.mkString("/", "/", ""))

}

object PorterExt extends ExtensionId[PorterExt] with ExtensionIdProvider {

  def createExtension(system: ExtendedActorSystem) = {
    val config = system.settings.config
    val settings = new PorterSettings(config.getConfig("porter"), system.dynamicAccess)
    val pa = system.actorOf(Props(classOf[PorterExtActor], settings), name = "porter-api")
    new PorterExt(pa, settings, system.provider.getDefaultAddress)
  }

  def lookup() = PorterExt

  class PorterExtActor(settings: PorterSettings) extends PorterActor with Actor {

    lazy val porter = new Porter {
      val store = new MultiStore {
        def stores = settings.stores
      }
      val authenticators = settings.authenticators

      settings.permissionFactories map register

      override def mutableStore(realm: Ident) = settings.findMutableStore(realm)
    }

    def receive = porterReceive orElse {
      case "show settings" => sender ! settings.toString
    }
  }
}

package porter.app.akka

import akka.actor._
import porter.app.PorterSettings
import porter.auth.Porter
import porter.store.MultiStore
import porter.model.Ident

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 19:52
 */
class PorterExt(val ref: ActorRef, val settings: PorterSettings) extends Extension {


}

object PorterExt extends ExtensionId[PorterExt] with ExtensionIdProvider {

  def createExtension(system: ExtendedActorSystem) = {
    val config = system.settings.config
    val settings = new PorterSettings(config.getConfig("porter"), system.dynamicAccess)
    val porter = new Porter {
      val store = new MultiStore {
        implicit def executionContext = system.dispatcher
        def stores = settings.stores
      }
      val authenticators = settings.authenticators
      override def mutableStore(realm: Ident) = settings.findMutableStore(realm)
    }
    val pa = system.actorOf(Props(classOf[PorterActor], porter), name = "porter-api")
    new PorterExt(pa, settings)
  }

  def lookup() = PorterExt

}

package porter.app.openid

import akka.actor.{ExtendedActorSystem, ExtensionIdProvider, ExtensionId, Extension}
import porter.app.akka.Porter

class OpenId(system: ExtendedActorSystem) extends Extension {
  private lazy val defaultAssocActor = system.actorOf(AssocActor())
  def defaultOpenIdService = {
    val porter = Porter(system).main
    OpenIdService(porter, defaultAssocActor, OpenIdSettings(system))
  }
}

object OpenId extends ExtensionId[OpenId] with ExtensionIdProvider {
  def lookup() = OpenId

  def createExtension(system: ExtendedActorSystem) = {
    new OpenId(system)
  }
}

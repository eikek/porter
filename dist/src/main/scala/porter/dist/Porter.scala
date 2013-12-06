package porter.dist

import akka.actor._

object Porter extends ExtensionId[Porter] with ExtensionIdProvider {
  def lookup() = Porter

  def createExtension(system: ExtendedActorSystem) =
    Porter(system.provider.getDefaultAddress, system)

}

case class Porter(address: Address, private val factory: ActorRefFactory) extends Extension {

  def porterPath(porter: ActorRef) = ActorPath.fromString(
    address.toString + porter.path.elements.mkString("/", "/", ""))
}

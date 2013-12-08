package porter.dist

import akka.actor._

object RemotePath extends ExtensionId[RemotePath] with ExtensionIdProvider {
  def lookup() = RemotePath

  def createExtension(system: ExtendedActorSystem) =
    RemotePath(system.provider.getDefaultAddress, system)
}

case class RemotePath(address: Address, private val factory: ActorRefFactory) extends Extension {

  def pathFor(porter: ActorRef) = ActorPath.fromString(
    address.toString + porter.path.elements.mkString("/", "/", ""))
}

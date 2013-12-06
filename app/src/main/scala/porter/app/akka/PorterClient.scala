package porter.app.akka

import akka.actor._
import scala.concurrent.Future
import akka.util.Timeout

class PorterClient(refFactory: ActorRefFactory) extends Extension {

  def remotePath(host: String, port: Int) =
    ActorPath.fromString(s"akka.tcp://porter@$host:$port/user/porter-api")

  def lookup(host: String = "localhost", port: Int = 4554)(implicit timeout: Timeout): Future[ActorRef] =
    refFactory.actorSelection(remotePath(host, port)).resolveOne()
}

object PorterClient extends ExtensionId[PorterClient] with ExtensionIdProvider {
  def lookup() = PorterClient

  def createExtension(system: ExtendedActorSystem) = new PorterClient(system)
}

package porter.app.akka.telnet

import akka.actor.{ActorLogging, Terminated, Props, Actor}
import akka.io.Tcp
import porter.app.akka.telnet.TelnetServer.GetConnCount

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 20:43
 */
class TelnetServer extends Actor with ActorLogging {

  private var connections = 0

  def receive = {
    case Tcp.Connected(_, _) =>
      val tcpConnection = sender
      val newchild = context.watch(context.actorOf(Props(classOf[TelnetConnection], tcpConnection, self)))
      connections += 1
      logConnections()
      sender ! Tcp.Register(newchild)

    case Terminated(_) =>
      connections -= 1
      logConnections()

    case GetConnCount =>
      sender ! connections
  }

  def logConnections() {
    log.info(s"There are currently $connections connections.")
  }
}

object TelnetServer {

  case object GetConnCount extends Serializable
}
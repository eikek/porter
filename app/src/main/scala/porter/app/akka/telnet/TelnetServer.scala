package porter.app.akka.telnet

import akka.actor._
import akka.io.{IO, Tcp}
import porter.app.akka.telnet.TelnetServer.GetConnCount
import akka.actor.Terminated
import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext
import akka.io.Tcp.Event
import akka.util.Timeout

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 20:43
 */
class TelnetServer(porter: ActorRef) extends Actor with ActorLogging {

  private var connections = 0

  def receive = {
    case Tcp.Connected(_, _) =>
      val tcpConnection = sender
      val newchild = context.watch(context.actorOf(TelnetConnection.props(porter, tcpConnection)))
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
    log.info(s"There are currently $connections telnet connections.")
  }
}

object TelnetServer {

  case object GetConnCount extends Serializable

  def props(porter: ActorRef) = Props(classOf[TelnetServer], porter)

  /**
   * Binds the telnet service to the given address. The Future is completed
   * with either [[akka.io.Tcp.Bound]] or [[akka.io.Tcp.CommandFailed]].
   *
   * @param host
   * @param port
   * @param system
   * @param ec
   * @return
   */
  def bind(porter: ActorRef, host: String, port: Int)
          (implicit system: ActorSystem, ec: ExecutionContext, bindTimeout: Timeout) = {
    import akka.pattern.ask
    val telnetEndpoint = new InetSocketAddress(host, port)
    val telnetServer = system.actorOf(props(porter), name = "porter-telnet")
    (IO(Tcp) ? Tcp.Bind(telnetServer, telnetEndpoint)).mapTo[Event]
  }
}
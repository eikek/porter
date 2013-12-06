package porter.app.akka.http

import akka.actor._
import spray.can.Http
import porter.app.akka.telnet.TelnetServer.GetConnCount
import akka.actor.Terminated
import scala.concurrent.ExecutionContext
import akka.io.{Tcp, IO}
import akka.util.Timeout

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 18:42
 */
class HttpHandler(porter: ActorRef) extends Actor with ActorLogging {
  
  var connections = 0

  def receive = {
    case Http.Bound(addr) =>
      log.info("Bound http interface to "+ addr)

    case Http.Connected(_, _) =>
      val newchild = context.watch(context.actorOf(HttpConnection.props(porter), name = s"httpconn$connections"))
      connections += 1
      logConnections()
      sender ! Http.Register(newchild)

    case Terminated(_) =>
      connections -= 1
      logConnections()

    case GetConnCount =>
      sender ! connections
  }

  def logConnections() {
    log.info(s"There are currently $connections http connections.")
  }
}

object HttpHandler {
  def props(porter: ActorRef) = Props(classOf[HttpHandler], porter)

  /**
   * Binds the http service to the given address. The Future is completed
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
    val httpHandler = system.actorOf(props(porter), name = "porter-http")
    (IO(Http) ? Http.Bind(httpHandler, interface = host, port = port)).mapTo[Tcp.Event]
  }
}
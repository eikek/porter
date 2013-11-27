package porter.app.akka.http

import akka.actor.{Terminated, Props, ActorLogging, Actor}
import spray.can.Http
import porter.app.akka.telnet.TelnetServer.GetConnCount

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 18:42
 */
class HttpHandler extends Actor with ActorLogging {
  
  var connections = 0

  def receive = {
    case Http.Bound(addr) =>
      log.info("Bound http interface to "+ addr)

    case Http.Connected(_, _) =>
      val newchild = context.watch(context.actorOf(Props(classOf[HttpConnection]), name = s"httpconn$connections"))
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

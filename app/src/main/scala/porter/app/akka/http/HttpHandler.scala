package porter.app.akka.http

import akka.actor._
import spray.can.Http
import akka.actor.Terminated
import porter.auth.Decider
import porter.model.PasswordCrypt

class HttpHandler(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) extends Actor with ActorLogging {
  import HttpHandler._

  var connections = 0

  def receive = {
    case Http.Bound(addr) =>
      log.info("Bound http interface to "+ addr)

    case Http.Connected(_, _) =>
      val newchild = context.watch(context.actorOf(HttpConnection(porter, decider, crypt), name = s"httpconn$connections"))
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

  case object GetConnCount extends Serializable

  def apply(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) =
    Props(classOf[HttpHandler], porter, decider, crypt)

}
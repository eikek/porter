package porter.app.openid

import akka.actor._
import spray.can.Http
import akka.actor.Terminated
import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import akka.io.{IO, Tcp}

class OpenIdHandler(porter: ActorRef, settings: OpenIdServiceSettings) extends Actor with ActorLogging {
  import OpenIdHandler._
  var connections = 0

  val assocActor = context.actorOf(AssocActor())
  val serviceProps = OpenIdService(porter, assocActor, settings)

  def receive = {
    case Http.Bound(addr) =>
      log.info("Bound http interface to "+ addr)

    case Http.Connected(_, _) =>
      val newchild = context.watch(context.actorOf(serviceProps, name = s"openidconn$connections"))
      connections += 1
      sender ! Http.Register(newchild)

    case Terminated(_) =>
      connections -= 1

    case GetConnCount =>
      sender ! connections
  }
}

object OpenIdHandler {
  case object GetConnCount

  def apply(porter: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdHandler], porter, settings)

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
  def bind(porter: ActorRef, settings: OpenIdServiceSettings, host: String, port: Int)
          (implicit system: ActorSystem, ec: ExecutionContext, bindTimeout: Timeout): Future[Tcp.Event] = {
    import akka.pattern.ask
    val httpHandler = system.actorOf(OpenIdHandler(porter, settings), name = "porter-openid")
    (IO(Http) ? Http.Bind(httpHandler, interface = host, port = port)).mapTo[Tcp.Event]
  }

}

package porter.app.openid

import akka.actor.{ActorLogging, Actor, Props, ActorRef}
import spray.routing.HttpServiceActor
import porter.app.openid.routes._
import akka.util.Timeout
import spray.http._
import akka.io.Tcp.ConnectionClosed

class OpenIdService(val porterRef: ActorRef, val assocActor: ActorRef, val settings: OpenIdServiceSettings) extends HttpServiceActor
  with ActorLogging
  with OpenIdActors
  with DiscoveryRoute
  with StaticRoute
  with ManageRoutes
  with EndpointRoute {

  implicit def dispatcher = context.dispatcher
  implicit val timeout = Timeout(3000)
  implicit val system = context.system

  def receive = runRoute {
    discovery ~ checkRoute ~ homeRoute ~ staticRoute
  }

  override def onConnectionClosed(ev: ConnectionClosed) = context.stop(self)
}

object OpenIdService {

  def apply(porter: ActorRef, assocActor: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdService], porter, assocActor, settings)

  def apply(porter: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdActor], porter, settings)

  class OpenIdActor(porter: ActorRef, settings: OpenIdServiceSettings) extends Actor {
    val assocActor = context.actorOf(AssocActor(), "association")
    val openid = context.actorOf(OpenIdService(porter, assocActor, settings), "service")
    def receive = {
      case m => openid.forward(m)
    }
  }
}

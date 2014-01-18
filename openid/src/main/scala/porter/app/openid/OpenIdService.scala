package porter.app.openid

import akka.actor.{ActorLogging, Actor, Props, ActorRef}
import spray.routing.HttpServiceActor
import porter.app.openid.routes._
import akka.util.Timeout
import spray.http._
import akka.io.Tcp.ConnectionClosed

class OpenIdService(val porterRef: ActorRef, val assocActor: ActorRef, val avatarRef: ActorRef, val settings: OpenIdServiceSettings) extends HttpServiceActor
  with ActorLogging
  with OpenIdActors
  with DiscoveryRoute
  with StaticRoute
  with ManageRoutes
  with EndpointRoute
  with AvatarRoute {

  implicit def dispatcher = context.dispatcher
  implicit val timeout = Timeout(3000)
  implicit val system = context.system

  def receive = runRoute {
    discovery ~ checkRoute ~ homeRoute ~ avatarRoute ~  staticRoute
  }

  override def onConnectionClosed(ev: ConnectionClosed) = context.stop(self)
}

object OpenIdService {

  def apply(porter: ActorRef, assocActor: ActorRef, avatarActor: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdService], porter, assocActor, avatarActor, settings)

  def apply(porter: ActorRef, avatarActor: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdActor], porter, avatarActor, settings)

  class OpenIdActor(porter: ActorRef, settings: OpenIdServiceSettings) extends Actor {
    val assocActor = context.actorOf(AssocActor(), "association")
    val openid = context.actorOf(OpenIdService(porter, assocActor, settings), "service")
    def receive = {
      case m => openid.forward(m)
    }
  }
}

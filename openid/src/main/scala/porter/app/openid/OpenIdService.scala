package porter.app.openid

import akka.actor.{Actor, Props, ActorRef}
import spray.routing.HttpServiceActor
import porter.app.openid.routes._
import akka.util.Timeout
import spray.http._

class OpenIdService(val porter: ActorRef, val assocActor: ActorRef, val settings: OpenIdServiceSettings) extends HttpServiceActor
  with Provides
  with AuthDirectives
  with PageDirectives
  with DiscoveryRoute
  with StaticRoute
  with HomeRoutes
  with EndpointRoute {

  implicit def dispatcher = context.dispatcher
  implicit val timeout = Timeout(3000)
  implicit val system = context.system

  def receive = runRoute {
    discovery ~ checkRoute ~ homeRoute ~ staticRoute
  }

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

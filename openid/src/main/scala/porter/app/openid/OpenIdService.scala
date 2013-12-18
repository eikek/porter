package porter.app.openid

import akka.actor.{Props, ActorRef}
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
  with EndpointRoute {

  implicit def dispatcher = context.dispatcher
  implicit val timeout = Timeout(3000)
  implicit val system = context.system

  def receive = runRoute {
    discovery ~ checkRoute ~ staticRoute
  }

}

object OpenIdService {

  def apply(porter: ActorRef, assocActor: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdService], porter, assocActor, settings)

}

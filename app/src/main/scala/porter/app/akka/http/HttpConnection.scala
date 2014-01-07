package porter.app.akka.http

import akka.actor._
import akka.util.Timeout
import spray.routing._
import porter.auth.Decider
import akka.io.Tcp.ConnectionClosed
import porter.model.PasswordCrypt

class HttpConnection(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) extends HttpServiceActor with ActorLogging {
  implicit val timeout = Timeout(5000)
  import context.dispatcher

  private val authRoute = AuthService(porter, decider).route
  private val storeRoute = StoreService(porter, decider, crypt).route

  def receive = runRoute {
    authRoute ~ storeRoute
  }

  override def onConnectionClosed(ev: ConnectionClosed) = context.stop(self)
}

object HttpConnection {
  def apply(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) = Props(classOf[HttpConnection], porter, decider, crypt)
}
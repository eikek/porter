package porter.app.akka.http

import akka.actor.Actor
import porter.app.akka.PorterExt
import akka.util.Timeout
import spray.can.Http
import spray.http.HttpRequest
import akka.actor.Terminated

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 18:55
 */
class HttpConnection extends Actor {
  import akka.pattern.pipe
  import context.dispatcher
  implicit val timeout = Timeout(5000)
  implicit val porter = PorterExt(context.system)
  context.watch(porter.ref)

  private val reqHandler = AuthRequests.all

  def receive = {
    case req: HttpRequest if reqHandler matches req.uri  =>
      val client = sender
      val f = reqHandler(ReqToken(req, porter, client))
      f.map(jsonResponse).recover(recoverResponse) pipeTo sender

    case Terminated(`porter`) =>
      context.stop(self)

    case x: Http.ConnectionClosed =>
      context.stop(self)
  }

}

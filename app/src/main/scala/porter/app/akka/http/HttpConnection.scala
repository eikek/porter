package porter.app.akka.http

import akka.actor._
import akka.util.Timeout
import spray.can.Http
import spray.http._
import spray.http.HttpRequest
import spray.http.HttpResponse
import akka.actor.Terminated

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 18:55
 */
private[http] class HttpConnection(porter: ActorRef) extends Actor with ActorLogging {
  import akka.pattern.pipe
  import context.dispatcher
  import _root_.porter.util.JsonHelper._
  implicit val timeout = Timeout(5000)

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

  def jsonResponse(data: Any): HttpResponse =
    HttpResponse(entity = HttpEntity(
      ContentTypes.`application/json`,
      toJsonString(data)), headers = defaultHeaders)

  def recoverResponse: PartialFunction[Throwable, HttpResponse] = {
    case x =>
      log.error(x, "Error processing client http request")
      HttpResponse(status = StatusCodes.BadRequest, entity = x.getMessage)
  }
}

object HttpConnection {
  def props(porter: ActorRef) = Props(classOf[HttpConnection], porter)
}
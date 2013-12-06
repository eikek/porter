package porter.app.akka.http

import akka.actor.{ActorLogging, Actor, Terminated}
import akka.util.Timeout
import spray.can.Http
import spray.http._
import spray.http.HttpRequest
import spray.http.HttpResponse
import porter.app.akka.api.Porter

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 18:55
 */
class HttpConnection extends Actor with ActorLogging {
  import akka.pattern.pipe
  import context.dispatcher
  import porter.util.JsonHelper._
  implicit val timeout = Timeout(5000)
  implicit val porterExt = Porter(context.system)
  context.watch(porterExt.singlePorter)

  private val reqHandler = AuthRequests.all

  def receive = {
    case req: HttpRequest if reqHandler matches req.uri  =>
      val client = sender
      val f = reqHandler(ReqToken(req, porterExt.singlePorter, client))
      f.map(jsonResponse).recover(recoverResponse) pipeTo sender

    case Terminated(`porterExt`) =>
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

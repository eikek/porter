package porter.app.akka.http

import akka.actor._
import akka.util.Timeout
import spray.can.Http
import spray.http._
import spray.http.HttpRequest
import spray.http.HttpResponse
import akka.actor.Terminated
import java.io.{PrintWriter, StringWriter}

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

  private val notfound = HttpResponse(
    status = StatusCodes.NotFound,
    entity = HttpEntity(ContentType(MediaTypes.`text/html`), "<html><h2>Not found</h2></html>")
  )

  def receive = {
    case req: HttpRequest if reqHandler matches req.uri  =>
      val client = sender
      val f = reqHandler(ReqToken(req, porter, client))
      f.map(jsonResponse).recover(recoverResponse) pipeTo sender

    case nf: HttpRequest =>
      sender ! notfound

    case Status.Failure(ex) =>
      sender ! recoverResponse(ex)

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
      val s = new StringWriter()
      x.printStackTrace(new PrintWriter(s))
      HttpResponse(
        status = StatusCodes.InternalServerError,
        entity = HttpEntity(ContentType(MediaTypes.`text/html`), s"<html><h2>Server Error</h2><pre>${s.toString}</pre></html>"))
  }
}

object HttpConnection {
  def props(porter: ActorRef) = Props(classOf[HttpConnection], porter)
}
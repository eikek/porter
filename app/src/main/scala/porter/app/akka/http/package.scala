package porter.app.akka

import spray.http._
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.ActorRef
import spray.http.HttpResponse
import akka.util.Timeout
import porter.model.Ident
import porter.auth.Vote

/**
 *
 * @author <a href="mailto:eike.kettner@gmail.com">Eike Kettner</a>
 * @since 27.11.13 21:07
 */
package object http {

  type Json = String
  trait Handler {
    def matches(path: Uri): Boolean
    def apply(token: ReqToken): Future[Any]
    def orElse(other: Handler): Handler = new Handler.OrElse(this, other)
  }
  object Handler {
    def apply(filter: Uri => Boolean)(pf: ReqToken => Future[Any]): Handler = new Handler {
      def matches(path: Uri) = filter(path)
      def apply(token: ReqToken) = pf(token)
    }

    private class OrElse(a: Handler, b: Handler) extends Handler {
      def matches(path: Uri) = a.matches(path) || b.matches(path)
      def apply(token: ReqToken) = if (a.matches(token.req.uri)) a(token) else b(token)
    }
  }

  val defaultHeaders = List(
    HttpHeaders.`Cache-Control`(CacheDirectives.`no-cache`)
  )

  def toJsonString(obj: Any): String = {
    obj match {
      case n: Number => n.toString
      case s: String => "\""+ s +"\""
      case b: Boolean => b.toString
      case pairs: Map[_, _] =>
        val elems = pairs.map({ case (k, v) => "\""+ k +"\": "+ toJsonString(v) })
        elems.mkString("{", ",", "}")
      case iter: Iterable[_] =>
        iter.map(toJsonString).mkString("[", ",", "]")
      case o => toJsonString(o.toString)
    }
  }

  case class ReqToken(req: HttpRequest, porter: PorterExt, sender: ActorRef)

  def jsonResponse(data: Any): HttpResponse =
    HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, toJsonString(data)), headers = defaultHeaders)

  def recoverResponse: PartialFunction[Throwable, HttpResponse] = {
    case x => HttpResponse(status = StatusCodes.BadRequest, entity = x.getMessage)
  }

}

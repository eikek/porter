package porter.app.akka.http

import spray.http._
import scala.concurrent.{ExecutionContext, Future}
import porter.model.{PasswordCredentials, Ident}
import akka.util.Timeout
import scala.util.parsing.json.{JSONArray, JSON, JSONObject}
import porter.app.akka.Porter

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 21:01
 */
object AuthRequests {
  import akka.pattern.ask
  import Porter.Messages.authc._
  import Porter.Messages.authz._

  def all(implicit ec: ExecutionContext, timeout: Timeout) = List(authz, authc).reduce(_ orElse _)

  def authz(implicit ec: ExecutionContext, timeout: Timeout) = onPath("/api/authz") { token =>
    token.req.entity.asString match {
      case JsonPerm(realm, login, perms) =>
        val f = (token.porter ? Authorize(realm, login, perms)).mapTo[AuthorizeResp]
        f.map(resp => Map("result" -> resp.authorized))
      case _ => Future.failed(new IllegalArgumentException("Bad authorization request: "+ token.req.entity.asString))
    }
  }

  def authc(implicit ec: ExecutionContext, timeout: Timeout) = onPath("/api/authc") { token =>
    import porter.util.JsonHelper._
    token.req.entity.asString match {
      case JsonAuth(realm, login, creds) =>
        val f = token.porter ? Authenticate(realm, Set(PasswordCredentials(login, creds)))
        f.mapTo[AuthenticateResp].map { resp =>
          resp.result.map(_.toJson).getOrElse(toJsonString(Map("result" -> "Invalid credentials")))
        }
      case _ => Future.failed(new IllegalArgumentException("Invalid request"))
    }
  }

  private def startsWith(path: String): Uri => Boolean = { uri =>
    uri.path.startsWith(Uri.Path(path))
  }

  private def onPath(path: String)(pf: ReqToken => Future[Any]) =
    Handler(startsWith(path))(pf)

  object JsonAuth {
    def unapply(in: String): Option[(Ident, Ident, String)] = {
      JSON.parseRaw(in) match {
        case Some(JSONObject(map)) =>
          (map.get("realm"), map.get("login"), map.get("password")) match {
            case (Some(r), Some(l), Some(c)) if Ident.isValid(l.toString) && Ident.isValid(r.toString) =>
              Some((r.toString, l.toString, c.toString))
            case _ => None
          }
        case _ => None
      }
    }
  }

  object JsonPerm {
    def unapply(in: String): Option[(Ident, Ident, Set[String])] = {
      JSON.parseRaw(in) match {
        case Some(JSONObject(map)) =>
          val r = map.get("realm").map(_.toString)
          val l = map.get("login").map(_.toString)
          val p = map.get("perms")
          (r, l, p) match {
            case (Some(realm), Some(login), Some(JSONArray(seq)))
              if Ident.isValid(realm) && Ident.isValid(login) && seq.nonEmpty =>
              Some(Ident(realm), Ident(login), seq.map(_.toString).toSet)
            case _ => None
          }
        case _ => None
      }
    }
  }
}

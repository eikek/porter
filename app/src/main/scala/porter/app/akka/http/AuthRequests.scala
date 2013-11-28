package porter.app.akka.http

import spray.http._
import scala.concurrent.{ExecutionContext, Future}
import porter.app.akka.PorterActor.{AuthResponse, Authenticate, AuthzResponse, Authorized}
import porter.model.{PasswordCredentials, Ident}
import akka.util.Timeout
import scala.util.parsing.json.{JSONArray, JSON, JSONObject}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 21:01
 */
object AuthRequests {
  import akka.pattern.ask

  def all(implicit ec: ExecutionContext, timeout: Timeout) = List(authz, authc).reduce(_ orElse _)

  def authz(implicit ec: ExecutionContext, timeout: Timeout) = onPath("/api/authz") { token =>
    token.req.entity.asString match {
      case JsonPerm(realm, login, perms) =>
        val f = (token.porter.ref ? Authorized(realm, login, perms)).mapTo[AuthzResponse]
        f.map(resp => Map("result" -> resp.result))
      case _ => Future.failed(new IllegalArgumentException("Bad authorization request: "+ token.req.entity.asString))
    }
  }

  def authc(implicit ec: ExecutionContext, timeout: Timeout) = onPath("/api/authc") { token =>
    import porter.util.JsonHelper._
    token.req.entity.asString match {
      case JsonAuth(realm, login, creds) =>
        val f = token.porter.ref ? Authenticate(realm, Set(PasswordCredentials(login, creds)))
        f.mapTo[AuthResponse].map { resp => resp.token.toJson }
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

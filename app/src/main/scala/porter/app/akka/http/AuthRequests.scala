package porter.app.akka.http

import spray.http._
import scala.concurrent.{ExecutionContext, Future}
import porter.app.akka.PorterActor.{AuthResponse, Authenticate, AuthzResponse, Authorized}
import porter.model.{PasswordCredentials, Ident}
import scala.util.{Failure, Success, Try}
import akka.util.Timeout
import scala.util.parsing.json.{JSON, JSONObject}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 21:01
 */
object AuthRequests {
  import akka.pattern.ask

  def all(implicit ec: ExecutionContext, timeout: Timeout) = List(authz, authc).reduce(_ orElse _)

  def authz(implicit ec: ExecutionContext, timeout: Timeout) = Handler(startsWith("/api/authz")) { token =>
    val uri = token.req.uri
    val req = Try(Ident(uri.query.get("realm").get),
      Ident(uri.query.get("login").get), uri.query.getAll("perms"))
    req match {
      case Success((realm, login, perms)) if perms.nonEmpty =>
        val f = (token.porter.ref ? Authorized(realm, login, perms.toSet)).mapTo[AuthzResponse]
        f.map(resp => Map("result" -> resp.result))
      case Failure(x) => Future.failed(x)
      case _ => Future.failed(new IllegalArgumentException("No permissions specified"))
    }
  }

  def authc(implicit ec: ExecutionContext, timeout: Timeout) = Handler(startsWith("/api/authc")) { token =>
    token.req.entity match {
      case body if body.nonEmpty =>
        body.asString match {
          case JsonAuth(realm, login, creds) =>
            val f = token.porter.ref ? Authenticate(realm, Set(PasswordCredentials(login, creds)))
            f.mapTo[AuthResponse].map { resp =>
              resp.token.votes.map({case (k, v) => s"vote.${k.name}" -> v }) ++ Map(
                "failed" -> resp.token.failedCount,
                "success" -> resp.token.successCount,
                "oneSuccess" -> resp.token.oneSuccess
              )
            }

          case _ => Future.failed(new IllegalArgumentException("Invalid request"))
        }
    }
  }

  def startsWith(path: String): Uri => Boolean = { uri =>
    uri.path.startsWith(Uri.Path(path))
  }

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
}

package porter.util

import java.net.{HttpURLConnection, URL, InetSocketAddress}
import porter.model.{Realm, Ident}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.parsing.json.{JSONObject, JSON}
import scala.io.Codec
import porter.auth.{Vote, AuthResult}

/**
 * Simple client for performing authentication and authorization on a remote
 * http porter instance.
 *
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 28.11.13 02:11
 */
class PorterHttp(addr: InetSocketAddress) {

  def this() = this(new InetSocketAddress("localhost", 6789))

  private def post[A](path: String, data: String)(f: String => A)(implicit ec: ExecutionContext): Future[A] = Future {
    val url = new URL("http", addr.getHostName, addr.getPort, path)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("POST")
    conn.setRequestProperty("Content-Type", "application/json; charset=UTF8")
    conn.setRequestProperty("Content-Length", data.getBytes.length.toString)
    conn.setUseCaches(false)
    conn.setDoInput(true)
    conn.setDoOutput(true)

    val out = conn.getOutputStream
    try {
      out.write(data.getBytes(Codec.UTF8.name))
      out.flush()
    } finally {
      out.close()
    }

    val in = conn.getInputStream
    val result = Try(io.Source.fromInputStream(in).getLines().mkString)
    in.close()
    result.map(f).get
  }

  def authorize(realm: Ident, login: Ident, perms: Set[String])(implicit ec: ExecutionContext): Future[Boolean] = {
    val req = porter.util.toJsonString(Map(
      "realm" -> realm.name,
      "login" -> login.name,
      "perms" -> perms
    ))

    post("/api/authz", req) { data =>
      JSON.parseRaw(data) match {
        case Some(JSONObject(map)) =>
          map.get("result").exists(x => x == true)
        case _ => false
      }
    }
  }

  def authenticate(realm: Ident, login: Ident, password: String)(implicit ec: ExecutionContext): Future[AuthResult] = {
    val req = porter.util.toJsonString(Map(
      "realm" -> realm.name,
      "login" -> login.name,
      "password" -> password
    ))

    def voteBool(v: Boolean): Vote = if (v) Vote.Success else Vote.Failed()

    post("/api/authc", req) { data =>
      JSON.parseRaw(data) match {
        case Some(JSONObject(map)) =>
          val props = map.get("account").flatMap {
            case JSONObject(bmap) => Some(bmap.map { case (k, v) =>
              k.toString -> v.toString
            })
            case _ => None
          }
          val votes = map.get("votes").flatMap {
            case JSONObject(bmap) => Some(bmap.map { case(k, v) =>
              Ident(k.toString) -> voteBool(v.toString.toBoolean)
            })
            case _ => None
          }
          val realm = map.get("realm").flatMap {
            case JSONObject(bmap) =>
              val id = bmap.get("id").map(_.toString)
              val name = bmap.get("name").map(_.toString)
              Try(Realm(id.get, name.get)).toOption
            case _ => None
          }
          (realm, props, votes) match {
            case (Some(r), Some(p), Some(v)) =>
              AuthResult(r, login, v, p)
            case _ =>
              throw new IllegalArgumentException("Invalid response: "+ data)
          }
        case _ =>
          throw new IllegalStateException("Invalid response: "+ data)
      }
    }
  }
}

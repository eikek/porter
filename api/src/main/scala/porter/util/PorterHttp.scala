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
 * instance using http.
 *
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 28.11.13 02:11
 */
class PorterHttp(addr: InetSocketAddress) {
  import porter.util.JsonHelper._

  def this() = this(new InetSocketAddress("localhost", 6789))

  private def post(path: String, data: String)(implicit ec: ExecutionContext): Future[String] = Future {
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
    result.get
  }

  def authorize(realm: Ident, login: Ident, perms: Set[String])(implicit ec: ExecutionContext): Future[Boolean] = {
    val req = toJsonString(Map(
      "realm" -> realm.name,
      "login" -> login.name,
      "perms" -> perms
    ))

    post("/api/authz", req) map { data =>
      JSON.parseRaw(data) match {
        case Some(JSONObject(map)) =>
          map.get("result").exists(x => x == true)
        case _ => false
      }
    }
  }

  def authenticate(realm: Ident, login: Ident, password: String)(implicit ec: ExecutionContext): Future[AuthResult] = {
    val req = toJsonString(Map(
      "realm" -> realm.name,
      "login" -> login.name,
      "password" -> password
    ))

    post("/api/authc", req) map {
      case JsonAuthResult(r) => r
      case data => throw new IllegalStateException("Invalid response: " + data)
    }
  }
}

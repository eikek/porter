package porter.client.http

import scala.concurrent.{Future, ExecutionContext}
import java.net.{InetSocketAddress, HttpURLConnection, URL}
import scala.io.Codec
import scala.util.Try

object Http {

  def post(addr: InetSocketAddress, path: String, data: String)
          (implicit ec: ExecutionContext): Future[String] = Future {
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

  def get(addr: InetSocketAddress, path: String)(implicit ec: ExecutionContext): Future[String] = Future {
    val url = new URL("http", addr.getHostName, addr.getPort, path)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("GET")
    conn.setUseCaches(false)
    conn.setDoOutput(true)
    val in = conn.getInputStream
    val result = Try(io.Source.fromInputStream(in).getLines().mkString)
    in.close()
    result.get
  }
}

package porter.client.http

import scala.concurrent.{Promise, Await, Future, ExecutionContext}
import java.net.{InetSocketAddress, HttpURLConnection, URL}
import scala.io.Codec
import scala.util.{Failure, Try}
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference

/**
 * For each request, a monitoring task is scheduled to run after the given timeout to investigate
 * the "inner" future. If it is not done, its thread is interrupted and the "outer" future (the
 * one returned to client code) is completed with an exception. The monitoring task is scheduled
 * with a [[java.util.concurrent.ScheduledExecutorService]] that has exactly one thread.
 */
object Http {
  private val timer = Executors.newScheduledThreadPool(1)

  private def withTimeout[A](f: Future[A], timeout: FiniteDuration, thread: AtomicReference[Thread])(implicit ec: ExecutionContext): Future[A] = {
    import concurrent.duration._
    val p = Promise[A]()
    val c = timer.schedule(new Runnable {
      def run() = {
        Try(Await.ready(f, -1.millis)) match {
          case Failure(ex) =>
            Try { thread.get().interrupt() }
            p.tryFailure(ex)
          case _ =>
        }
      }
    }, timeout.length, timeout.unit)
    f.onComplete { c.cancel(true); p.tryComplete }
    p.future
  }

  def post(addr: InetSocketAddress, path: String, data: String)
          (implicit ec: ExecutionContext, to: FiniteDuration): Future[String] = {

    val thread = new AtomicReference[Thread]()
    val worker = Future {
      thread.set(Thread.currentThread())
      val url = new URL("http", addr.getHostName, addr.getPort, path)
      val conn = url.openConnection().asInstanceOf[HttpURLConnection]
      try {
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
        thread.set(null)
        in.close()
        result.get
      } finally {
        Try(conn.disconnect())
      }
    }
    withTimeout(worker, to, thread)
  }

  def get(addr: InetSocketAddress, path: String)
         (implicit ec: ExecutionContext, to: FiniteDuration): Future[String] = {
    val thread = new AtomicReference[Thread]()
    val worker = Future {
      thread.set(Thread.currentThread())
      val url = new URL("http", addr.getHostName, addr.getPort, path)
      val conn = url.openConnection().asInstanceOf[HttpURLConnection]
      try {
        conn.setRequestMethod("GET")
        conn.setUseCaches(false)
        conn.setDoOutput(true)
        val in = conn.getInputStream
        val result = Try(io.Source.fromInputStream(in).getLines().mkString)
        thread.set(null)
        in.close()
        result.get
      } finally {
        conn.disconnect()
      }
    }
    withTimeout(worker, to, thread)
  }
}

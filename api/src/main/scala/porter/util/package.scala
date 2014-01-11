package porter

import java.io.{FileInputStream, BufferedInputStream, File}
import scala.util.{Failure, Success, Try}
import scala.util.parsing.json.{JSONType, JSONObject, JSON}
import porter.model.{Properties, Realm, Ident}
import porter.auth.{Vote, AuthResult}
import scala.concurrent.Future

package object util {

  import java.util.{Properties => JProperties}

  implicit class FutureAdds(val f: Future.type) extends AnyVal {
    def immediate[A](v: Try[A]): Future[A] = v match {
      case Success(e) => Future.successful(e)
      case Failure(x) => Future.failed(x)
    }

    def immediate[A](v: Option[A], name: String = ""): Future[A] = v match {
      case Some(e) => Future.successful(e)
      case None => Future.failed(new NoSuchElementException(name))
    }

    def immediate[A](value: => A): Future[A] =
      immediate(Try(value))
  }

  object Properties {

    def toMap(props: JProperties): Map[String, String] = {
      import scala.collection.JavaConverters._
      val buf = collection.mutable.ListBuffer[(String, String)]()
      for (k <- props.stringPropertyNames().asScala) {
        buf append (k -> props.getProperty(k))
      }
      buf.toMap
    }

    def fromPairs(values: Iterable[(String, String)]): JProperties = {
      val p = new JProperties()
      for ((k,v) <- values) p.setProperty(k, v)
      p
    }

    def fromPairs(values: (String, String)*): JProperties = {
      val p = new JProperties()
      for ((k,v) <- values) p.setProperty(k, v)
      p
    }

    def fromFile(f: File): Try[JProperties] = Try {
      val p = new JProperties()
      val in = new BufferedInputStream(new FileInputStream(f))
      try { p.load(in) }
      finally { in.close() }
      p
    }
  }
}

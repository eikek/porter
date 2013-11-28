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

  object JsonHelper {

    private def voteBool(v: Boolean): Vote = if (v) Vote.Success else Vote.Failed()

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

    object RealmObject {
      def unapply(obj: JSONType): Option[Realm] = obj match {
        case JSONObject(bmap) =>
          val id = bmap.get("id").map(_.toString)
          val name = bmap.get("name").map(_.toString)
          Try(Realm(id.get, name.get)).toOption
        case _ => None
      }
    }
    object VotesObject {
      def unapply(obj: JSONType): Option[Map[Ident, Vote]] =
        obj match {
          case JSONObject(bmap) => Some(bmap.map { case(k, v) =>
            Ident(k.toString) -> voteBool(v.toString.toBoolean)
          })
          case _ => None
        }
    }
    object PropertiesObject {
      def unapply(obj: JSONType): Option[Properties] = obj match {
        case JSONObject(bmap) => Some(bmap.map { case (k, v) =>
          k.toString -> v.toString
        })
        case _ => None
      }
    }

    object JsonAuthResult {
      def unapply(json: String): Option[AuthResult] = {
        JSON.parseRaw(json) match {
          case Some(JSONObject(map)) =>
            val props = map.get("account") match {
              case Some(PropertiesObject(p)) => Some(p)
              case _ => None
            }
            val votes = map.get("votes") match {
              case Some(VotesObject(v)) => Some(v)
              case _ => None
            }
            val realm = map.get("realm") match {
              case Some(RealmObject(r)) => Some(r)
              case _ => None
            }
            (realm, props, votes) match {
              case (Some(r), Some(p), Some(v)) =>
                val login = Try(Ident(p.get("login").get)).toOption
                login map { AuthResult(r, _, v, p - "login") }
              case _ => None
            }
          case _ => None
        }
      }
    }

    implicit class AuthResultJson(res: AuthResult) {
      def toJson = Map(
        "votes" -> res.votes.map({case (k, v) => s"${k.name}" -> v.toBoolean }),
        "failed" -> res.failedCount,
        "success" -> res.successCount,
        "oneSuccess" -> res.oneSuccess,
        "realm" -> Map("id" -> res.realm.id.name, "name" -> res.realm.name),
        "account" -> (res.props + ("login" -> res.accountId))
      )
    }
  }
}

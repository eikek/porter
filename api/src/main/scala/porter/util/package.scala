package porter

import java.io.{FileInputStream, BufferedInputStream, File}
import scala.util.Try

package object util {

  import java.util.{Properties => JProperties}

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
}

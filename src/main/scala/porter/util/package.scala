package porter

import java.io.{FileInputStream, BufferedInputStream, File}
import scala.util.Try

/**
 *
 * @since 22.11.13 21:25
 *
 */
package object util {

  import java.util.{Properties => JProperties}

  object Properties {

    def toMap(props: JProperties): Map[String, String] = {
      import scala.collection.JavaConverters._
      val buf = collection.mutable.ListBuffer[(String, String)]()
      for (k <- props.propertyNames().asScala) {
        buf append (k.toString -> props.getProperty(k.toString))
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

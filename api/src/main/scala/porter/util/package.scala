/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

  /**
   * Splits the given string around the given separator char, returning
   * a list without empty strings
   * @param str
   * @param sep
   * @return
   */
  def split(str: String, sep: Char): List[String] =
    str.split(sep).collect({ case s if s.trim.nonEmpty => s.trim }).toList

}

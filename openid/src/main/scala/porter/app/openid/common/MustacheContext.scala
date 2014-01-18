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

package porter.app.openid.common

import java.text.DateFormat
import Mustache.Context

/**
 * Helps with populating [[scala.collection.Map]]s for use with [[porter.app.openid.common.Mustache]]
 */
object MustacheContext {

  val empty = Map.empty[String, Any]
  val buildInfoMap = {
    import porter.BuildInfo._
    Map("porter" ->
      Map("version" -> version,
        "revision" -> revision,
        "builtTime" -> DateFormat.getDateInstance.format(new java.util.Date(buildTime))
      )
    )
  }

  object Data {
    def append[T](value: T)(implicit conv: MapConverter[T]): Context => Context =
      _ ++ unwrapOption(value.toMap)
    def appendRaw(value: Context): Context => Context = _ ++ unwrapOption(value)
  }

  class KeyedData(name: String) {
    def put[T](value: T)(implicit conv: ValueConverter[T]): Context => Context =
      _ ++ unwrapOption(Map(name -> value.toValue))
    def putIf[T](cond: => Boolean, value: T)(implicit conv: ValueConverter[T]): Context => Context =
      if (cond) put(value)(conv) else identity

    def putRaw(value: Context): Context => Context = _.updated(name, unwrapOption(value))
    def putRaw(value: List[Any]): Context => Context = _.updated(name, value)
    def putPairList(value: Context): Context => Context = putRaw(pairList(value))
  }
  object KeyedData {
    def apply(name: String) = new KeyedData(name)
  }

  private def pairList(data: Context): List[Context] = {
    (data map { case (k, v) => Map("name" -> k, "value" -> v) }).toList
  }

  private def unwrapOption(m: Map[String, Any]): Map[String, Any] = {
    m.filterNot(_._2 == None).mapValues {
      case Some(x) => x
      case m: Map[_, _] => unwrapOption(m.asInstanceOf[Map[String, Any]])
      case l: List[_] => l.headOption match {
        case Some(m: Map[_, _]) => l.map(m => unwrapOption(m.asInstanceOf[Map[String, Any]]))
        case _ => l
      }
      case x => x
    }
  }
  trait ValueConverter[T] {
    def convert(obj: T): Any
  }
  trait MapConverter[T] extends ValueConverter[T] {
    def convert(obj: T): Map[String, Any]
  }

  implicit class ToContextValue[T](obj: T) {
    def toValue(implicit conv: ValueConverter[T]) = conv.convert(obj)
    def toMap(implicit conv: MapConverter[T]) = conv.convert(obj)
  }

  trait BasicConverter {
    final class PassConv[T] extends ValueConverter[T] {
      def convert(obj: T) = obj
    }
    implicit val stringConv = new PassConv[String]
    implicit val intConv = new PassConv[Int]
    implicit val longConv = new PassConv[Long]
    implicit val boolConv = new PassConv[Boolean]

    implicit def optionValueConv[T](implicit conv: ValueConverter[T]) = new ValueConverter[Option[T]] {
      def convert(obj: Option[T]) = obj match {
        case Some(x) => x.toValue
        case _ => None
      }
    }
    implicit def listConv[T](implicit conv: ValueConverter[T]) = new ValueConverter[List[T]] {
      def convert(obj: List[T]) = conv match {
        case mc: MapConverter[T] => obj.map(v => unwrapOption(v.toMap(mc)))
        case _ => obj.map(_.toValue)
      }
    }
    implicit def mapConv[T](implicit conv: ValueConverter[T]) = new ValueConverter[Map[String, T]] {
      def convert(obj: Map[String, T]) = unwrapOption(obj.mapValues(_.toValue))
    }
  }
  object BasicConverter extends BasicConverter
}

package porter.model

import porter.util.Base64

trait Property[T] extends PropertyView[T] {
  def set(value: T): Properties => Properties = setRaw(value.toString)
  def setRaw(value: String): Properties => Properties = _.updated(name, value)
  def getRaw(map: Properties): Option[String] = map.get(name)
  def remove: Properties => Properties = _ - name
}
trait PropertyView[T] {
  def name: String
  def get(map: Properties): Option[T]
}

object Property {
  case class BoolProperty(name: String) extends Property[Boolean] {
    def get(map: Properties) = map.get(name).map(_.toBoolean)
    def toTrue = set(true)
    def toFalse = set(false)
    def isTrue(map: Properties) = get(map).getOrElse(false)
  }
  case class StringProperty(name: String) extends Property[String] {
    def get(map: Properties) = map.get(name)
  }
  case class IntProperty(name: String) extends Property[Int] {
    def get(map: Properties) = map.get(name).map(_.toInt)
  }
  case class LongProperty(name: String) extends Property[Long] {
    def get(map: Properties) = map.get(name).map(_.toLong)
  }
  case class CounterProperty(name: String) extends Property[Int] {
    def get(map: Properties) = map.get(name).map(_.toInt)
    def reset = remove
    def increment: Properties => Properties = map => set(get(map).getOrElse(0) + 1)(map)
    def decrement: Properties => Properties = map => {
      get(map) match {
        case Some(i) if i <= 1 => remove(map)
        case Some(i) if i > 1 => set(i -1)(map)
        case None => map
      }
    }
  }
  case class TimestampProperty(name: String) extends Property[java.util.Date] {
    private def iso = {
      val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
      df.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
      df
    }
    override def set(value: java.util.Date): Properties => Properties = _.updated(name, iso.format(value))
    def get(map: Properties) = map.get(name).map(iso.parse)
    def current = set(new java.util.Date())
    def getMillis(map: Properties) = get(map).map(_.getTime)
    def getString(map: Properties) = map.get(name)
  }
  case class BinaryValue(contentType: String, data: Array[Byte])
  case class BinaryProperty(name: String) extends Property[BinaryValue] {
    override def set(value: BinaryValue) = setRaw(value.contentType+":"+Base64.encode(value.data))
    def get(map: Properties) = {
      getRaw(map).flatMap { raw =>
        porter.util.split(raw, ':') match {
          case ct :: data :: Nil => Some(BinaryValue(ct, Base64.decode(data).toArray))
          case _ => None
        }
      }
    }
  }

  case class Concat(name: String, separator: String, props: Iterable[PropertyView[String]]) extends PropertyView[String] {
    def get(map: Properties) = {
      val s = (StringBuilder.newBuilder /: props) { (sb, p) =>
        p.get(map).map(v => sb.append(v).append(separator))
        sb
      }
      if (s.isEmpty) None else Some(s.toString())
    }
  }
}
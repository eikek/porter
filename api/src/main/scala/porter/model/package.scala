package porter

package object model {
  import scala.language.implicitConversions

  type Properties = Map[String, String]

  type Parts = List[Set[String]]

  type Rule = Either[Revocation, Permission]

  type PermissionFactory = PartialFunction[String, Permission]

  implicit def permRule(p:Permission): Rule = Right(p)
  implicit def revocRule(r:Revocation): Rule = Left(r)

  def partitionRules(rules: Iterable[Rule]): (Set[Permission], Set[Revocation]) = {
    val (perm, revocs) = rules.partition(_.isRight)
    (perm.map(_.right.get).toSet, revocs.map(_.left.get).toSet)
  }

  trait Property[T] {
    def name: String
    def setRaw(value: String): Properties => Properties = _.updated(name, value)
    def getRaw(map: Properties): Option[String] = map.get(name)
    def set(value: T): Properties => Properties = setRaw(value.toString)
    def get(map: Properties): Option[T]
    def remove: Properties => Properties = _ - name
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
  }
}
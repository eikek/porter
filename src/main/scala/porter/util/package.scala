package porter

/**
 *
 * @since 22.11.13 21:25
 *
 */
package object util {

  implicit class PropertiesPimped(props: java.util.Properties) {
    def propList(key: String) =
      Option(props.getProperty(key)).getOrElse("").split(',').map(_.trim).filterNot(_.isEmpty)

    def prop(key: String) =
      Option(props.getProperty(key))
        .getOrElse(throw new IllegalArgumentException(s"Invalid store. Cannot find property '$key'."))

    def propMap(key: String): Map[String, String] = (for {
        kv <- propList(key)
        pair <- List(kv.split("\\Q->\\E"))
        if pair.length == 2
      } yield pair(0).trim -> pair(1).trim).toMap

  }
}

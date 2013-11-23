package porter.store

import java.io.File
import scala.util.Try

/**
 * Simple store based on java's properties files.
 *
 * It only supports accounts with one password crypted using bcrypt.
 *
 * Example:
 * {{{
 *   porter.realms=realm1,realm2
 *   porter.realm1.name=myrealm
 *   porter.realm2.name=another realm
 *   porter.realm1.accounts=john,mary
 *   porter.realm1.account.john.secret=bcrypt(password)
 *   porter.realm1.account.john.groups=g1,g2,g3
 *   porter.realm1.account.john.props=enabled -> true, email -> email@test.org
 *   porter.realm1.groups=admin,g1,g2
 *   porter.realm1.group.admin.rules=printer:manage:*, !printer:manage:superprinter
 * }}}
 *
 * @since 22.11.13 19:41
 */
class PropertiesStore(props: Map[String, String]) extends SimpleStore {
  import porter.model._
  import PropertiesStore._

  lazy val realms = (for (id <- props.propList("porter.realms"))
    yield Realm(id, props.prop(s"porter.$id.name"))).toList

  lazy val groups =
    for {
      realm <- realms
      name <- props.propList(s"porter.${realm.id.name}.groups")
    } yield realm -> Group(
      name,
      props.propMap(s"porter.${realm.id.name}.group.$name.props"),
      props.propList(s"porter.${realm.id.name}.group.$name.rules").toSet
    )

  lazy val accounts =
    for {
      realm <- realms
      name <- props.propList(s"porter.${realm.id.name}.accounts")
    } yield realm -> Account(
      name,
      props.propMap(s"porter.${realm.id.name}.account.$name.props"),
      props.propList(s"porter.${realm.id.name}.account.$name.groups").toSet.map(Ident.apply),
      Seq(Secret(Secret.Types.bcrypt, props.prop(s"porter.${realm.id.name}.account.$name.secret")))
    )
}

object PropertiesStore {
  import porter.util._
  import java.util.{Properties => JProperties}

  def apply(props: Map[String, String]): PropertiesStore = new PropertiesStore(props)
  def apply(props: java.util.Properties): PropertiesStore = new PropertiesStore(props.toMap)
  def apply(file: File): Try[PropertiesStore] = Properties.fromFile(file).map(apply)

  private implicit class StoreMap(map: Map[String, String]) {
    def prop(key: String) = map.get(key)
        .getOrElse(throw new IllegalArgumentException(s"Invalid store. Cannot find property '$key'."))

    def propList(key: String) =
      map.get(key).getOrElse("").split(',').map(_.trim).filterNot(_.isEmpty)

    def propMap(key: String): Map[String, String] = (for {
      kv <- propList(key)
      pair <- List(kv.split("\\Q->\\E"))
      if pair.length == 2
    } yield pair(0).trim -> pair(1).trim).toMap
  }

  private implicit class JPropertiesAdds(props: JProperties) {
    def toMap = Properties.toMap(props)
  }
}
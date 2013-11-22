package porter.store

import java.util.Properties
import scala.util.{Success, Try}

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
 *   porter.realm1.group.admin.permissions=printer:manage:*, !printer:manage:superprinter
 * }}}
 *
 * @since 22.11.13 19:41
 */
class PropertiesStore(props: Properties) extends Store {
  import porter.model._
  import porter.util._

  lazy val realms = (for (id <- props.propList("porter.realms"))
    yield Realm(id, props.prop(s"porter.$id.name"))).toList

  lazy val groups =
    for {
      realm <- realms
      name <- props.propList(s"porter.${realm.id.name}.groups")
    } yield realm -> Group(
      name,
      props.propMap(s"porter.${realm.id.name}.group.$name.props"),
      props.propList(s"porter.${realm.id.name}.group.$name.permissions").toSet
    )

  lazy val accounts =
    for {
      realm <- realms
      name <- props.propList(s"porter.${realm.id.name}.accounts")
    } yield realm -> Account(
      name,
      props.propMap(s"porter.${realm.id.name}.account.$name.props"),
      props.propList(s"porter.${realm.id.name}.account.$name.groups").toSet.map(Ident.apply),
    //TODO make this secret name somehow available, add feature to quickly generate those password
      Seq(Secret("bcrypt-password", props.prop(s"porter.${realm.id.name}.account.$name.secret")))
    )

  def findRealms(names: Set[Ident]) = Try {
    realms filter (r => names.contains(r.id))
  }

  def findAccounts(realm: Ident, names: Set[Ident]) = Try {
    for {
      (r, a) <- accounts
      if r.id == realm && names.contains(a.name)
    } yield a
  }

  def findGroups(realm: Ident, names: Set[Ident]) = Try {
    for {
      (r, g) <- groups
      if r.id == realm && names.contains(g.name)
    } yield g
  }

  def allRealms() = Success(realms)

  def allAccounts(realm: Ident) = Try(for ((r,a) <- accounts; if r.id == realm) yield a)

  def allGroups(realm: Ident) = Try(for ((r,g) <- groups; if r.id == realm) yield g)
}

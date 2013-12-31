package porter.app

import porter.store.SimpleStore
import com.typesafe.config.Config
import scala.util.Try

/**
 * Example:
 * {{{
 *   cfg {
 *     realm1: {
 *       name: "my great realm"
 *       groups: {
 *         admin: {
 *           rules: [ "bla.bla", "!bla.bla.blop" ]
 *           props: {
 *             key: "value"
 *           }
 *         }
 *         ...
 *       }
 *       accounts: {
 *         john: {
 *           secret: "cryptedstring"
 *           groups: [ "admin", "user" ]
 *           props: {
 *             key: "value"
 *           }
 *         }
 *       }
 *     }
 *   }
 * }}}
 *
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 18:51
 */
class ConfigStore(cfg: Config) extends SimpleStore {
  import collection.JavaConverters._
  import porter.model._

  val realms = (for (k <- cfg.root().keySet().asScala)
    yield Realm(k, cfg.getString(s"$k.name"))).toList

  val groups = for {
      r <- realms
      g <- cfg.getConfig(s"${r.id.name}.groups").root().keySet().asScala
    } yield r -> Group(
      g,
      Try(getProps(cfg.getConfig(s"${r.id.name}.groups.$g.props"))).getOrElse(Map.empty),
      cfg.getStringList(s"${r.id.name}.groups.$g.rules").asScala.toSet
    )

  val accounts = for {
      r <- realms
      a <- cfg.getConfig(s"${r.id.name}.accounts").root().keySet().asScala
    } yield r -> Account(
      a,
      Try(getProps(cfg.getConfig(s"${r.id.name}.accounts.$a.props"))).getOrElse(Map.empty),
      cfg.getStringList(s"${r.id.name}.accounts.$a.groups").asScala.map(Ident.apply).toSet,
      Seq(Password.crypted(cfg.getString(s"${r.id.name}.accounts.$a.secret")))
    )

  private def getProps(cfg: Config): Properties =
    (for (k <- cfg.root().keySet().asScala) yield k -> cfg.getString(k)).toMap


}

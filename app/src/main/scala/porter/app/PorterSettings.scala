package porter.app

import _root_.akka.actor.{ReflectiveDynamicAccess, DynamicAccess}
import com.typesafe.config.Config
import scala.reflect.ClassTag
import scala.util.Try
import porter.auth.Authenticator
import porter.store.{MutableStore, Store}
import porter.model.{PermissionFactory, Permission, Ident}

trait PorterSettings {
  def authenticators: List[Authenticator]
  def stores: List[Store]
  def mutableStores: List[(Set[Ident], MutableStore)]
  def permissionFactories: List[PermissionFactory]

  override def toString = {
    val as = authenticators.mkString(" ", "\n ", "")
    val ss = stores.mkString(" ", "\n ", "")
    val ms = mutableStores.mkString(" ", "\n ", "")
    val pfs = permissionFactories.mkString(" ", "\n ", "")
    s"""
      |Authenticators
      |--------------
      |$as
      |
      |Stores
      |------
      |$ss
      |
      |MutableStores
      |-------------
      |$ms
      |
      |PermissionFactories
      |-------------------
      |$pfs
    """.stripMargin
  }
}

object PorterSettings {
  private val dynamicAccess = new ReflectiveDynamicAccess(classOf[PorterSettings].getClassLoader)

  /**
   * Example configuration:
   * {{{
   *   myporter {
   *     authenticators: [
   *        { class = "com.package.MyAuthenticator1", params = {} }
   *     ]
   *     //read-only and mutable stores are specified
   *     stores: [
   *       {
   *         class = "com.package.MyStore"
   *         params { ... }
   *         # for a mutable store, specify which realms it is for
   *         realms = [ "realm1", "realm2" ]
   *       },
   *       {
   *         class = "porter.akka.ConfigStore"
   *         params = ${samplestore}
   *       }
   *       {
   *         class = "porter.akka.PropertyFileStore"
   *         params = { file: "/var/accounts.properties" }
   *       }
   *     ]
   *     permissionFactories: [
   *       { class: "com.package.SomePermissionFactory", params: {} }
   *     ]
   *     samplestore {
   *       // see ConfigStore api doc
   *     }
   *   }
   * }}}
   *
   */
  def fromConfig(cfg: Config, dynamicAccess: DynamicAccess = dynamicAccess): PorterSettings =
    new ConfigPorterSettings(cfg, dynamicAccess)


  private class ConfigPorterSettings(cfg: Config, dynamicAccess: DynamicAccess = new ReflectiveDynamicAccess(classOf[PorterSettings].getClassLoader)) extends PorterSettings {
    import scala.collection.JavaConverters._

    val authenticators = {
      val authMaker = makeInstance[Authenticator](dynamicAccess)_
      val list = cfg.getConfigList("authenticators").asScala
      for (c <- list) yield authMaker(c).get
    }.toList

    private val storeObjects = {
      val storeMaker = makeInstance[AnyRef](dynamicAccess)_ andThen(_.get)
      cfg.getConfigList("stores").asScala.map(c => c -> storeMaker(c)).toList
    }

    val stores = storeObjects.collect({ case (_, s: Store) => s }).toList

    val mutableStores = storeObjects.collect {
      case (c, s: MutableStore) => optGet(c, "realms").map(Ident.apply).toSet -> s
    }

    val permissionFactories = {
      val maker = makeInstance[PermissionFactory](dynamicAccess)_
      val list = cfg.getConfigList("permissionFactories").asScala
      for (c <- list) yield maker(c).get
    }.toList

    private def optGet(cfg: Config, key: String): List[String] =
      Try { cfg.getStringList(key).asScala.toList } getOrElse List.empty

    private def makeInstance[T: ClassTag](dynAccess: DynamicAccess)(cfg: Config) = {
      val className = cfg.getString("class")
      val params = cfg.getConfig("params")
      createInstanceOf(dynAccess, params)(className)
    }

    private def createInstanceOf[T: ClassTag](dynAccess: DynamicAccess, cfg: Config)(fqcn: String): Try[T] = {
      lazy val loadObject = dynAccess.getObjectFor(fqcn)
      val args = scala.collection.immutable.Seq.empty[(Class[_], AnyRef)]
      lazy val configCtor = dynAccess.createInstanceFor(fqcn, args :+ (classOf[Config] -> cfg))
      val defctor = dynAccess.createInstanceFor(fqcn, args)

      configCtor orElse defctor orElse loadObject
    }
  }

}
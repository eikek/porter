package porter.app.akka

import com.typesafe.config.Config
import porter.auth.{Authenticator, PasswordAuthenticator, Porter}
import porter.store.{MultiStore, MutableStore, Store, SimpleStore}
import porter.model._
import porter.model.Group
import porter.model.Realm
import porter.model.Account
import akka.actor.{DynamicAccess, ReflectiveDynamicAccess}
import scala.reflect.{ClassTag, classTag}
import scala.util.{Success, Try}
import scala.collection.mutable.ListBuffer

/**
 * Creates an instance of `Porter` using the given configuration. Things are
 * wired by instantiating FQCNs inside the config.
 *
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
 *         realms = []
 *       }
 *     ]
 *
 *     samplestore {
 *       realms = [ "realm1" ]
 *       accounts = [
 *         john = {
 *           groups = [ "g2", "g1" ]
 *           secret = "bcryptstring"
 *           props = {
 *              key: value
 *           }
 *         }
 *       ]
 *   }
 * }}}
 *
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 16:27
 */
class ConfiguredPorter(config: Config) extends Porter {

  private val settings = ConfiguredPorter.fromConfig(config, getClass.getClassLoader).get

  val store = new MultiStore {
    implicit def executionContext = ???
    val stores = settings.stores
  }

  val authenticators = settings.authenticators

  override def mutableStore(realm: Ident) = {
    settings.mstores.find(ms => ms.realms.contains(realm)).map(_.store)
  }
}

object ConfiguredPorter {

  case class MStore(realms: Set[Ident], store: MutableStore)
  case class Settings(authenticators: List[Authenticator], stores: List[Store], mstores: List[MStore])

  def fromConfig(cfg: Config, loader: ClassLoader): Try[Settings] = Try {
    val dynamicAccess = new ReflectiveDynamicAccess(loader)
    val authMaker = makeInstance[Authenticator](dynamicAccess)_
    val storeMaker = makeInstance[AnyRef](dynamicAccess)_

    import scala.collection.JavaConverters._

    val list = cfg.getConfigList("authenticators").asScala
    val authers = for (c <- list) yield authMaker(c).get
    val storeObjList = for (s <- cfg.getConfigList("stores").asScala)
      yield (s.getStringList("realms").asScala.toSet.map(Ident.apply), storeMaker(s))
    val stores = ListBuffer[Store]()
    val mstores = ListBuffer[MStore]()
    for ((realms, Success(obj)) <- storeObjList) {
      obj match {
        case s:Store => stores append s
        case _ =>
      }
      obj match {
        case ms: MutableStore => mstores append MStore(realms, ms)
        case _ =>
      }
    }
    Settings(
      authers.toList,
      stores.toList,
      mstores.toList
    )
  }

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

    loadObject orElse configCtor orElse defctor
  }
}
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

package porter.app

import _root_.akka.actor.{ActorSystem, ReflectiveDynamicAccess, DynamicAccess}
import com.typesafe.config.Config
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import porter.auth.Validator
import porter.store.{MutableStore, Store}
import porter.model.{PermissionFactory, Permission, Ident}

trait PorterSettings {
  def validators: List[Validator]
  def stores: List[Store]
  def mutableStores: List[(Set[Ident], MutableStore)]
  def permissionFactories: List[PermissionFactory]

  override def toString = {
    val as = validators.mkString(" ", "\n ", "")
    val ss = stores.mkString(" ", "\n ", "")
    val ms = mutableStores.mkString(" ", "\n ", "")
    val pfs = permissionFactories.mkString(" ", "\n ", "")
    s"""
      |Validators
      |----------
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
   *     validators: [
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
  def fromConfig(system: ActorSystem, cfg: Config, dynamicAccess: DynamicAccess = dynamicAccess): PorterSettings =
    new ConfigPorterSettings(system, cfg, dynamicAccess)

  private class ConfigPorterSettings(system: ActorSystem, cfg: Config, dynamicAccess: DynamicAccess = new ReflectiveDynamicAccess(classOf[PorterSettings].getClassLoader)) extends PorterSettings {
    import scala.collection.JavaConverters._

    val validators = {
      val authMaker = makeInstance[Validator](dynamicAccess)_
      val list = cfg.getConfigList("validators").asScala
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
      lazy val systemCtor = dynAccess.createInstanceFor(fqcn, args :+ (classOf[ActorSystem] -> system))
      lazy val sysconfCtor = dynAccess.createInstanceFor(fqcn, args :+ (classOf[ActorSystem] -> system) :+ (classOf[Config] -> cfg))
      val defctor = dynAccess.createInstanceFor(fqcn, args)

      configCtor orElse sysconfCtor orElse systemCtor orElse defctor orElse loadObject match {
        case r @ Success(_) => r
        case Failure(ex) => Failure(new Exception(s"Unable to create instance: $fqcn", ex))
      }
    }
  }

}
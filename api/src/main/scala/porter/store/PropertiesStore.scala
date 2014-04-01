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

package porter.store

import java.io.File
import scala.util.Try
import porter.util.Properties
import porter.model.PropertyList

/**
 * Simple store based on java's properties files.
 *
 * It only supports accounts with one crypted password.
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
 */
class PropertiesStore(props: Map[String, String]) extends SimpleStore {
  import porter.model._
  import PropertiesStore._

  def this(filename: String) = this(Properties.toMap(Properties.fromFile(new java.io.File(filename)).get))
  def this(props: java.util.Properties) = this(Properties.toMap(props))

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
      Seq(Password.crypted(props.prop(s"porter.${realm.id.name}.account.$name.secret")))
    )
}

object PropertiesStore {
  import porter.util._
  import java.util.{Properties => JProperties}

  def apply(props: Map[String, String]): PropertiesStore = new PropertiesStore(props)
  def apply(props: java.util.Properties): PropertiesStore = new PropertiesStore(props)
  def apply(file: File): Try[PropertiesStore] = Properties.fromFile(file).map(apply)

  private implicit class StoreMap(map: Map[String, String]) {
    def prop(key: String) = map.get(key)
        .getOrElse(throw new IllegalArgumentException(s"Invalid store. Cannot find property '$key'."))

    def propList(key: String) =
      map.get(key).getOrElse("").split(',').map(_.trim).filterNot(_.isEmpty)

    def propMap(key: String): Map[String, String] =  {
      val props = (for {
        kv <- propList(key)
        pair <- List(kv.split("\\Q->\\E"))
        if pair.length == 2
      } yield pair(0).trim -> pair(1).trim).toMap
      PropertyList.mutableSource.toFalse(props)
    }
  }
}
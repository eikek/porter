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

package porter.app.openid.routes

import porter.model.{PropertyList, Property, Account}
import porter.app.openid.common.LocalId
import spray.http.{DateTime, Uri}
import porter.app.openid.routes.manage.Message
import porter.app.openid.OpenIdServiceSettings
import org.eknet.spray.openid.provider.MustacheContext
import java.util.Locale

trait Templating extends MustacheContext.MoreConverter {
  import MustacheContext._

  object KeyName {
    val localId = KeyedData("localId")
    val loginFailed = KeyedData("loginFailed")
    val params = KeyedData("params")
    val registerUrl = KeyedData("registerUrl")
    val adminProps = KeyedData("adminProps")
    val properties = KeyedData("properties")
    val account = KeyedData("account")
    val actionUrl = KeyedData("actionUrl")
    val loginUrl = KeyedData("loginUrl")
    val withEmail = KeyedData("withEmail")
    val withKey = KeyedData("withKey")
    val registerFailed = KeyedData("registerFailed")
    val registerFailedReasons = KeyedData("registerFailedReasons")
    val fields = KeyedData("fields")
    val avatarUrl = KeyedData("avatarUrl")
    val infoMessage = KeyedData("infoMessage")
    val staticResourcesPath = KeyedData("staticResourcesPath")
  }

  val buildInfoMap = {
    import porter.BuildInfo._
    val values = KeyedData("version").put(version)
      .andThen(KeyedData("revision").put(revision))
      .andThen(KeyedData("builtTime").put(DateTime(buildTime).toIsoLikeDateTimeString))
    KeyedData("porter").putRaw(values(empty))
  }

  implicit object AccountConv extends MapConverter[Account] {
    def convert(account: Account) = {
      Map(
        "name" -> account.name.name,
        "props" -> account.props,
        "groups" -> account.groups.map(_.name).toList,
        "secrets" -> account.secrets.toList.map(_.name.name)
      )
    }
  }

  case class FileField(name: String, label: String)
  implicit object FileFieldConv extends MapConverter[FileField] {
    def convert(obj: Templating.this.type#FileField) = {
      val map = TextField(obj.name, obj.label, None).toMap
      map.updated("type", "file")
    }
  }

  //when using a case class, the generated unapply method issues a compiler warning, because
  //the Property[_] inside the Option[(...)] results in inferring an existential type. I therefore
  //chose to omit a case class here
  class PropertyField(val prop: Property[_], val label: String, val account: Account, val locale: Locale)
  object PropertyField {
    def apply(prop: Property[_], label: String, account: Account, locale: Locale) =
      new PropertyField(prop, label, account, locale)
  }
  implicit object PropertyFieldConv extends MapConverter[PropertyField] {
    def convert(obj: PropertyField) = {
      val value = obj.prop.getRaw(obj.account.props)
      obj.prop match {
        case PropertyList.locale =>
          LocaleField(obj.prop.name, obj.label, value, obj.locale).toMap
        case PropertyList.timezone =>
          TimezoneField(obj.prop.name, obj.label, value, obj.locale).toMap
        case PropertyList.avatar =>
          FileField(obj.prop.name, obj.label).toMap
        case _ =>
          TextField(obj.prop.name, obj.label, value).toMap
      }
    }
  }

  implicit object LocalIdConv extends ValueConverter[LocalId] {
    def convert(obj: LocalId) = Map(
      "realm" -> obj.realm.name,
      "account" -> obj.account.name
    )
  }

  implicit object MessageConv extends MapConverter[Message] {
    def convert(obj: Message) = {
      if (obj.isEmpty) null
      else Map(
        "level" -> obj.level,
        "message" -> obj.text,
        "messageItems" -> obj.items
      )
    }
  }


  def defaultContext(settings: OpenIdServiceSettings) = {
    import Implicits._
    val ctx = buildInfoMap.andThen(KeyName.staticResourcesPath.put(settings.openIdUrl.toRelative.appendPath("/statics")))
    ctx(empty)
  }
}

object Templating extends Templating

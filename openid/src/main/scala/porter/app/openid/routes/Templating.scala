package porter.app.openid.routes

import porter.app.openid.common._
import porter.model.{PropertyList, Property, Account}
import java.util.{TimeZone, Locale}
import porter.model.Account
import porter.app.openid.common.Key
import scala.Some
import porter.app.openid.common.RequestedAttributes
import porter.app.openid.common.LocalId
import spray.http.Uri
import MustacheContext._
import porter.app.openid.routes.manage.Message
import porter.app.openid.OpenIdServiceSettings

trait Templating extends MustacheContext.BasicConverter {

  object KeyName {
    val endpointUrl = KeyedData("endpointUrl")
    val localId = KeyedData("localId")
    val loginFailed = KeyedData("loginFailed")
    val params = KeyedData("params")
    val registerUrl = KeyedData("registerUrl")
    val returnToUrl = KeyedData("returnToUrl")
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

  case class TextField(name: String, label: String, value: Option[String])
  implicit object TextFieldConv extends MapConverter[TextField] {
    def convert(obj: TextField) = {
      Map(
        "name" -> obj.name,
        "label" -> obj.label,
        "value" -> obj.value,
        "type" -> "text"
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

  case class SelectField(name: String, label: String, value: Option[String], values: List[(String, String)])
  implicit object SelectFieldConv extends MapConverter[SelectField] {
    def convert(obj: SelectField) = {
      val options = for ((id, name) <- obj.values)
      yield Map("id" -> id, "name" -> name, "selected" -> (obj.value == Some(id)))
      TextField(obj.name, obj.label, obj.value).toValue match {
        case m: Map[_, _] => m.asInstanceOf[Map[String, Any]] ++ Map("values" -> options, "select" -> true)
        case _ => sys.error("Expected map object")
      }
    }
  }

  case class LocaleField(name: String, label: String, value: Option[String], locale: Locale)
  implicit object LocaleFieldConv extends MapConverter[LocaleField] {
    def convert(obj: LocaleField) = {
      val allLocales =
        for (l <- Locale.getAvailableLocales)
        yield l.toLanguageTag -> l.getDisplayName(obj.locale)

      SelectField(obj.name, obj.label, obj.value, ("", "None") :: allLocales.toList).toMap
    }
  }

  case class TimezoneField(name: String, label: String, value: Option[String], locale: Locale)
  implicit object TimezoneFieldConv extends MapConverter[TimezoneField] {
    def convert(obj: TimezoneField) = {
      val tzs =
        for (id <- TimeZone.getAvailableIDs) yield {
          val tz = TimeZone.getTimeZone(id)
          val name = tz.getDisplayName(obj.locale) +" ("+id+")"
          id -> name
        }
      SelectField(obj.name, obj.label, obj.value, ("", "None") :: tzs.toList).toMap
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

  case class AttributeValues(attr: RequestedAttributes, account: Account, locale: Locale)
  implicit object RequestedAttributesConv extends MapConverter[AttributeValues] {
    def convert(obj: AttributeValues) = {
      val conversion = keyToMap(obj.account, obj.locale)_
      val opts = obj.attr.optional.map(conversion).map(_.updated("required", false))
      val reqs = obj.attr.required.map(conversion).map(_.updated("required", true))
      Map(
        "policy_url" -> obj.attr.url,
        "attributesExist" -> obj.attr.nonEmpty,
        "attributes" -> (reqs ++ opts)
      )
    }
    private val mapping = Map(
      SRegAttributes.email -> PropertyList.email,
      SRegAttributes.fullname -> PropertyList.fullName,
      SRegAttributes.language -> PropertyList.locale,
      SRegAttributes.timezone -> PropertyList.timezone,
      SRegAttributes.country -> PropertyList.country
    )

    def keyToMap(account: Account, locale: Locale)(key: Key) = {
      val value = mapping.get(key).flatMap(p => p.get(account.props))
      key match {
        case SRegAttributes.language =>
          LocaleField(key.openid, key.name, value, locale).toMap
        case SRegAttributes.timezone =>
          TimezoneField(key.openid, key.name, value, locale).toMap
        case SRegAttributes.gender =>
          SelectField(key.openid, key.name, None, List(""-> "None", "M" -> "Male", "F" -> "Female")).toMap
        case _ =>
          TextField(key.openid, key.name, value).toMap
      }
    }
  }

  implicit object UriConv extends ValueConverter[Uri] {
    def convert(obj: Uri) = obj.toString()
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
    val ctx = KeyName.staticResourcesPath.put(settings.openIdUrl.toRelative.appendPath("/statics"))
    ctx(MustacheContext.buildInfoMap)
  }
}

object Templating extends Templating

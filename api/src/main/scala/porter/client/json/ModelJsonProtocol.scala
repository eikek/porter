package porter.client.json

import spray.json._
import porter.model._
import porter.util.{Base64, AES}
import porter.auth.Vote
import scala.util.{Try, Success, Failure}

trait ModelJsonProtocol extends DefaultJsonProtocol {
  implicit val identFormat = new JsonFormat[Ident] {
    def write(obj: Ident) = StringJsonFormat.write(obj.name)
    def read(json: JsValue) = Ident.fromString(StringJsonFormat.read(json)) match {
      case Some(id) => id
      case x => deserializationError("Expected identifier string as JsString, but got " + x)
    }
  }

  implicit val passwordCredentialsFormat = new RootJsonFormat[PasswordCredentials] {
    case class Userpass(accountName: Ident, password: String) extends PasswordCredentials
    val helper = jsonFormat2(Userpass)
    def write(obj: PasswordCredentials) = helper.write(Userpass(obj.accountName, obj.password))
    def read(json: JsValue) = helper.read(json)
  }

  implicit val derivedCredentialsFormat = new RootJsonFormat[DerivedCredentials] {
    case class EncodedDerviedCredentials(key: String, data: String)
    private val helper = jsonFormat2(EncodedDerviedCredentials)
    def write(obj: DerivedCredentials) = {
      val key = AES.generateRandomKey
      val encoded = EncodedDerviedCredentials(Base64.encode(key), obj.encode(key))
      helper.write(encoded)
    }
    def read(json: JsValue) = {
      val encoded = helper.read(json)
      val key = Base64.decode(encoded.key).toVector
      DerivedCredentials.tryDecode(key, encoded.data) match {
        case Success(c) => c
        case Failure(ex) => deserializationError("Invalid credentials", ex)
      }
    }
  }

  implicit object DigestQopFormat extends JsonFormat[DigestQop] {
    val dauthFormat = jsonFormat(DigestAuth, "cnonce", "nc")
    val dauthintFormat = jsonFormat(DigestAuthInt, "cnonce", "nc", "requestmd5")
    def write(obj: DigestQop) = obj match {
      case da: DigestAuth => dauthFormat.write(da)
      case da: DigestAuthInt => dauthintFormat.write(da)
    }
    def read(json: JsValue) =
      Try(dauthintFormat.read(json)).getOrElse(dauthFormat.read(json))
  }
  implicit val digestCredentialsFormat = jsonFormat(DigestCredentials, "accountName", "method", "uri", "nonce", "response", "qop")


  implicit object CredentialsFormat extends RootJsonFormat[Credentials] {
    def write(obj: Credentials) = obj match {
      case pc: PasswordCredentials => passwordCredentialsFormat.write(pc)
      case dc: DerivedCredentials => derivedCredentialsFormat.write(dc)
      case dc: DigestCredentials => digestCredentialsFormat.write(dc)
      case x => serializationError("Don't know how to convert credentials: " +x)
    }
    def read(json: JsValue) = {
      val tryc = Try(passwordCredentialsFormat.read(json))
        .orElse(Try(derivedCredentialsFormat.read(json)))
        .orElse(Try(digestCredentialsFormat.read(json)))
      tryc match {
        case Success(c) => c
        case Failure(ex) => deserializationError("Unable to read Credentials from "+ json, ex)
      }
    }
  }

  implicit object SecretFormat extends RootJsonFormat[Secret] {
    case class JsSecret(name: Ident, data: String)
    val helper = jsonFormat2(JsSecret)
    def write(obj: Secret) = {
      val jssecret = JsSecret(obj.name, Base64.encode(obj.data))
      helper.write(jssecret)
    }
    def read(json: JsValue) = {
      val jssecret = helper.read(json)
      Secret(jssecret.name, Base64.decode(jssecret.data).toVector)
    }
  }

  implicit object VoteFormat extends RootJsonFormat[Vote] {
    case class VoteMap(vote: String, success: Boolean, reasons: Map[Ident, String])
    val helper = jsonFormat3(VoteMap)
    def write(obj: Vote) = {
      val map = obj match {
        case Vote.Success => VoteMap("success", true, Map.empty)
        case Vote.Failed(reasons) => VoteMap("failed", false, reasons)
      }
      helper.write(map)
    }
    def read(json: JsValue) = {
      val map = helper.read(json)
      if (map.success) Vote.Success
      else Vote.Failed(map.reasons)
    }
  }

  implicit val realmFormat = jsonFormat(Realm, "id", "name")
  implicit val accountFormat = jsonFormat(Account, "name", "props", "groups", "secrets")
  implicit val groupFormat = jsonFormat(Group, "name", "props", "rules")

}
object ModelJsonProtocol extends ModelJsonProtocol

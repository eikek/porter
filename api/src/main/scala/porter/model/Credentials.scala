package porter.model

import scala.concurrent.duration.Duration
import porter.util.{Hash, AES, Base64}
import scala.util.Try

@SerialVersionUID(20131122)
trait Credentials extends Serializable

@SerialVersionUID(20131122)
trait AccountCredentials extends Credentials {
  def accountName: Ident
}

@SerialVersionUID(20131216)
trait PasswordCredentials extends AccountCredentials {
  def password: String
  override final def toString = s"${getClass.getSimpleName}(${accountName.name}, ***)"
}

@SerialVersionUID(20131122)
object PasswordCredentials {

  def apply(accountName: Ident, password: String): PasswordCredentials = UserPass(accountName, password)

  def unapply(cred: Credentials): Option[(Ident, String)] = cred match {
    case pc: PasswordCredentials => Some((pc.accountName, pc.password))
    case _ => None
  }

  @SerialVersionUID(20131216)
  private case class UserPass(accountName: Ident, password: String) extends PasswordCredentials

}

@SerialVersionUID(20131130)
case class DigestCredentials(accountName: Ident,
                             method: String,
                             uri: String,
                             serverNonce: String,
                             response: String,
                             qop: Option[DigestQop] = None) extends AccountCredentials

@SerialVersionUID(20131130)
sealed trait DigestQop extends Serializable {
  def name: String
  def cnonce: String
  def nonceCount: String
}
@SerialVersionUID(20131130)
case class DigestAuthInt(cnonce: String, nonceCount: String, requestMd5: String) extends DigestQop {
  val name = "auth-int"
}
@SerialVersionUID(20131130)
case class DigestAuth(cnonce: String, nonceCount: String) extends DigestQop {
  val name = "auth"
}

@SerialVersionUID(20131201)
case class DerivedCredentials private (accountName: Ident, secret: Secret, valid: Long) extends AccountCredentials {

  def isExpired = valid > 0 && valid < System.currentTimeMillis()
  def encode(key: Vector[Byte]): String = {
    // account$secret.name$until$duration$data
    val b = StringBuilder.newBuilder
    b append accountName.name
    b append "$"
    b append secret.name.name
    b append "$"
    b append valid
    b append "$"
    b append Base64.encode(secret.data)
    val str = b.toString()
    AES.encryptString(str, key)
  }
}

object DerivedCredentials {

  def apply(account: Ident, secret: Secret, validFor: Duration = Duration.Inf): DerivedCredentials = {
    val valid =
      if (validFor == Duration.Inf) -1
      else validFor.toMillis + System.currentTimeMillis()
    DerivedCredentials(account, Secret(secret.name, Hash.sha512(secret.data)), valid)
  }

  def tryDecode(key: Vector[Byte], data: String) = decode(key, data).map {
    case (id, secret, valid) => DerivedCredentials(id, secret, valid)
  }

  private def decode(key: Vector[Byte], data: String) = Try {
    val decrypt = AES.decryptString(data, key)
    decrypt.split('$').toList match {
      case acc::secname::until::secdata::Nil =>
        (Ident(acc), Secret(secname, Base64.decode(secdata).toVector), until.toLong)
      case _ => throw new IllegalArgumentException("Wrong credentials")
    }
  }
}
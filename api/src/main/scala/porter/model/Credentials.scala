package porter.model

import scala.concurrent.duration.Duration
import porter.util.{AES, Base64}
import scala.util.{Success, Failure, Try}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 19:20
 */
@SerialVersionUID(20131122)
trait Credentials extends Serializable

@SerialVersionUID(20131122)
trait AccountCredentials extends Credentials {
  def accountName: Ident
}

@SerialVersionUID(20131122)
case class PasswordCredentials(accountName: Ident, password: String) extends AccountCredentials {
  override def toString = s"${getClass.getSimpleName}(${accountName.name}, ***)"
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
case class DerivedCredentials(key: Vector[Byte], data: String) extends AccountCredentials {

  private val fields = decode

  private[this] def decode = Try {
    val decrypt = AES.decryptString(data, key)
    decrypt.split('$').toList match {
      case acc::secname::until::secdata::Nil =>
        (Ident(acc), Secret(secname, Base64.decode(secdata).toVector), until.toLong)
      case _ => throw new IllegalArgumentException("Wrong credentials")
    }
  }

  def account = fields map { case (a, _, _) => a }
  def secret = fields map { case (_, s, _) => s }
  def expires = fields map { case (_, _, ts) => ts }
  def isExpired = expires match {
    case Success(ts) => ts > 0 && ts <= System.currentTimeMillis()
    case _ => true
  }

  def accountName = account.get
}

object DerivedCredentials {

  def apply(account: Ident, secret: Secret, validFor: Duration = Duration.Inf)(key: Vector[Byte]): DerivedCredentials = {
    import scala.concurrent.duration._
    // account$secret.name$until$duration$data
    val b = new StringBuilder(account.name)
    b append "$"
    b append secret.name.name
    b append "$"
    if (validFor == Duration.Inf) b append -1
    else b append (validFor.toMillis + System.currentTimeMillis())
    b append "$"
    b append Base64.encode(secret.data)
    DerivedCredentials(key, AES.encryptString(b.toString(), key))
  }


}
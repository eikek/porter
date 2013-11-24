package porter.model

import com.lambdaworks.crypto.SCryptUtil
import org.mindrot.jbcrypt.BCrypt

@SerialVersionUID(20131121)
case class Secret(name: Ident, data: Vector[Byte]) extends Serializable {
  import scala.io.Codec
  override def toString = s"Secret(${name.name}, ***)"
  lazy val asString = new String(data.toArray, Codec.UTF8.name)
}

object Secret {
  import scala.io.Codec
  import java.security.SecureRandom
  import javax.crypto.SecretKeyFactory
  import javax.crypto.spec.PBEKeySpec
  import porter.util.Base64

  private val random = SecureRandom.getInstance("SHA1PRNG")

  private def intBelow(n: Int) = (random.nextDouble() * n).toInt

  def apply(name: Ident, data: String): Secret = Secret(name, data.getBytes(Codec.UTF8.name).toVector)
  def apply(name: Ident, data: Array[Byte]): Secret = Secret(name, data.toVector)


  object Types {
    val bcrypt = Ident("bcrypt-password")
    val scrypt = Ident("scrypt-password")
    val pbkdf = Ident("pbkdf2sha1-password")
    val all = Set(bcrypt, scrypt, pbkdf)
  }

  def pbkdf2PasswordWithParams(pw: String, salt: Vector[Byte], n: Int) = {
    val len = 160
    val enc = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
      .generateSecret(new PBEKeySpec(pw.toCharArray, salt.toArray, n, len))
      .getEncoded
    Secret(Types.pbkdf, "$pbkdf2-sha1$"+n+"$"+ Base64.encode(salt) +"$"+Base64.encode(enc))
  }

  def pbkdf2Password(pw: String) = {
    val salt = new Array[Byte](16)
    random.nextBytes(salt)
    val iterations = 44000 + intBelow(10000)
    pbkdf2PasswordWithParams(pw, salt.toVector, iterations)
  }

  def scryptPassword(pw: String, n: Int = math.pow(2, 11 + intBelow(7)).toInt, r: Int = 16, p: Int = 2) =
    Secret(Types.scrypt, SCryptUtil.scrypt(pw, n, r, p))

  def bcryptPassword(pw: String, salt: String = BCrypt.gensalt(10 + intBelow(5))): Secret =
    Secret(Types.bcrypt, BCrypt.hashpw(pw, salt))

}

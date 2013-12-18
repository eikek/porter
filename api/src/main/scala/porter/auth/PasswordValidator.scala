package porter.auth

import org.mindrot.jbcrypt.BCrypt
import com.lambdaworks.crypto.SCryptUtil
import porter.util.Base64
import java.security.MessageDigest
import scala.io.Codec

/**
 * @since 22.11.13 22:07
 */
object PasswordValidator extends Validator {
  import porter.model._
  import Password._

  def authenticate(token: AuthToken) = {
    val userpass = token.credentials collect { case c: PasswordCredentials => c }
    val secrets = token.account.secrets.filter(_.name.name.startsWith("password."))
    userpass.foldLeft(token) { (token, up) =>
      secrets.foldLeft(token) { (token, sec) =>
        if (checkPassword(up.password, sec.asString)) token.vote(sec -> Vote.Success)
        else token.vote(sec -> Vote.Failed())
      }
    }
  }

  @scala.annotation.tailrec
  private def checkPassword(plain: String, hashed: String): Boolean = hashed match {
    case Bcrypt(hash) => BCrypt.checkpw(plain, hash)
    case Scrypt(hash) => SCryptUtil.check(plain, hash)
    case Pbkdf2(iter, salt, data) =>
      val enc = Pbkdf2(salt, iter)(plain)
      enc == hashed
    case Digest(algo, iter, salt, data) => Digest(algo, iter, salt)(plain) == hashed
    case h if h startsWith "$shiro1$" =>
      checkPassword(plain, h.replace("$shiro1$", "digest$"))
    case _ => false
  }
}

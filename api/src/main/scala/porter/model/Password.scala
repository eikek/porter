package porter.model

import org.mindrot.jbcrypt.BCrypt
import java.security.{MessageDigest, SecureRandom}
import com.lambdaworks.crypto.SCryptUtil
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import porter.util.Base64
import scala.io.Codec
import scala.util.Try
import java.util.concurrent.atomic.AtomicInteger
import porter.model.PasswordCrypt.Bcrypt

/**
 * Factory for password secrets. Usage:
 * {{{
 *   import porter.model.Password
 *   import porter.model.Password._
 *   Password("test")
 *   Password(Scrypt())("test")
 *   Password("password.1", Bcrypt(12))("test")
 * }}}
 *
 */
object Password {
  private def secretName(num: Int) = Ident(s"password.$num")

  def create(name: Ident, crypt: PasswordCrypt)(plain: String): Secret = Secret(name, crypt(plain))

  def apply = create(secretName(0), Bcrypt())_
  def apply(name: Ident) = create(name, Bcrypt())_
  def apply(crypt: PasswordCrypt) = create(secretName(0), crypt)_
  def apply(name: Ident, crypt: PasswordCrypt) = create(name, crypt)_

  def verify(plain: String, hashed: String): Boolean = hashed match {
    case PasswordCrypt(verifier) => verifier(plain)
    case _ => false
  }
}

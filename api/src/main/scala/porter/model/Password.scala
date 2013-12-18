package porter.model

import org.mindrot.jbcrypt.BCrypt
import java.security.{MessageDigest, SecureRandom}
import com.lambdaworks.crypto.SCryptUtil
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import porter.util.Base64
import scala.io.Codec
import scala.util.Try

/**
 * Factory for password secrets. Usage:
 * {{{
 *   Password("test")
 *   Password(Password.Scrypt())("test")
 * }}}
 *
 */
object Password {
  private val random = SecureRandom.getInstance("SHA1PRNG")
  private def intBelow(n: Int) = (random.nextDouble() * n).toInt
  private def randomSalt: Vector[Byte] = {
    val salt = new Array[Byte](16)
    random.nextBytes(salt)
    salt.toVector
  }
  private def split(str: String, c: Char = '$') =
    str.split(c).collect({ case s if s.trim.nonEmpty => s.trim }).toList

  type Crypt = String => String

  object Bcrypt {
    val name = "bcrypt"
    def apply(): Crypt = apply(10 + intBelow(5))
    def apply(strength: Int): Crypt = apply(BCrypt.gensalt(strength))
    def apply(salt: String): Crypt = new Crypt {
      def apply(plain: String) = name +"$"+BCrypt.hashpw(plain, salt)
    }

    def unapply(enc: String): Option[String] =
      if (enc startsWith (name+"$")) Some(enc.substring(name.length+1))
      else None
  }

  object Scrypt {
    val name = "scrypt"
    def apply(n: Int, r: Int, p: Int): Crypt = new Crypt {
      def apply(plain: String) = name+"$"+SCryptUtil.scrypt(plain, n, r, p)
    }
    def apply(): Crypt = apply(math.pow(2, 11 + intBelow(7)).toInt, 16, 2)

    def unapply(enc: String): Option[String] =
      if (enc startsWith (name+"$")) Some(enc.substring(name.length+1))
      else None
  }

  object Pbkdf2 {
    val name = "pbkdf2-sha1"
    def apply(): Crypt = {
      val iterations = 44000 + intBelow(10000)
      apply(randomSalt.toVector, iterations)
    }

    def apply(salt: Vector[Byte], n: Int): Crypt = new Crypt {
      def apply(plain: String) = {
        val len = 160
        val enc = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
          .generateSecret(new PBEKeySpec(plain.toCharArray, salt.toArray, n, len))
          .getEncoded
        name +"$"+n+"$"+ Base64.encode(salt) +"$"+Base64.encode(enc)
      }
    }
    def unapply(enc: String):Option[(Int, Vector[Byte], String)] = split(enc) match {
      case `name`::iter::salt::data::Nil => Try(
        (iter.toInt, Base64.decode(salt).toVector, data)
      ).toOption
      case _ => None
    }
  }

  object Digest {
    val name = "digest"
    def md5(n: Int = 200000, salt: Vector[Byte] = randomSalt): Crypt = apply("MD5", n, salt)
    def sha1(n: Int = 200000, salt: Vector[Byte] = randomSalt): Crypt = apply("SHA-1", n, salt)
    def sha256(n: Int = 200000, salt: Vector[Byte] = randomSalt): Crypt = apply("SHA-256", n, salt)
    def sha512(n: Int = 200000, salt: Vector[Byte] = randomSalt): Crypt = apply("SHA-512", n, salt)

    def apply(algorithm: String, n: Int, salt: Vector[Byte]): Crypt = new Crypt {
      def apply(plain: String) = {
        val bytes = plain.getBytes(Codec.UTF8.charSet)
        val md = MessageDigest.getInstance(algorithm)
        md.reset()
        md.update(salt.toArray)
        var hashed = md.digest(bytes)
        for (i <- 1 until n) {
          md.reset()
          hashed = md.digest(hashed)
        }
        name+"$"+algorithm+"$"+n+"$"+Base64.encode(salt) +"$"+ Base64.encode(hashed)
      }
    }

    def unapply(enc: String): Option[(String, Int, Vector[Byte], String)] = split(enc) match {
      case `name`::algo::iter::salt::data::Nil => Try(
        (algo, iter.toInt, Base64.decode(salt).toVector, data)
      ).toOption
      case _ => None
    }
  }

  def create(name: Ident, crypt: Crypt)(plain: String): Secret = Secret(name, crypt(plain))

  def apply = create("password.0", Bcrypt())_
  def apply(name: Ident) = create(name, Bcrypt())_
  def apply(crypt: Crypt) = create("password.0", crypt)_
  def apply(name: Ident, crypt: Crypt) = create(name, crypt)_
}

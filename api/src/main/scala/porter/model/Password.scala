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
  private val _random = SecureRandom.getInstance("SHA1PRNG")
  private val _counter = new AtomicInteger(0)
  private def random = {
    if (_counter.compareAndSet(10000, 0)) {
      _random.setSeed(SecureRandom.getSeed(32))
    }
    _counter.incrementAndGet()
    _random
  }
  private def intBelow(n: Int) = (random.nextDouble() * n).toInt
  private def randomSalt: Vector[Byte] = {
    val salt = new Array[Byte](16)
    random.nextBytes(salt)
    salt.toVector
  }
  private def split(str: String, c: Char = '$') =
    str.split(c).collect({ case s if s.trim.nonEmpty => s.trim }).toList

  def secretName(num: Int) = Ident(s"password.$num")

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
    def apply(n: Int = math.pow(2, 11 + intBelow(7)).toInt, r: Int = 16, p: Int = 2): Crypt = new Crypt {
      def apply(plain: String) = name+"$"+SCryptUtil.scrypt(plain, n, r, p)
    }
    def unapply(enc: String): Option[String] =
      if (enc startsWith (name+"$")) Some(enc.substring(name.length+1))
      else None
  }

  object Pbkdf2 {
    val name = "pbkdf2-sha1"

    def apply(salt: Vector[Byte] = randomSalt, n: Int = 54000 + intBelow(10000)): Crypt = new Crypt {
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
    def md5(n: Int = 500000, salt: Vector[Byte] = randomSalt): Crypt = apply("MD5", n, salt)
    def sha1(n: Int = 500000, salt: Vector[Byte] = randomSalt): Crypt = apply("SHA-1", n, salt)
    def sha256(n: Int = 500000, salt: Vector[Byte] = randomSalt): Crypt = apply("SHA-256", n, salt)
    def sha512(n: Int = 500000, salt: Vector[Byte] = randomSalt): Crypt = apply("SHA-512", n, salt)

    def apply(algorithm: String, n: Int = 200000, salt: Vector[Byte] = randomSalt): Crypt = new Crypt {
      def apply(plain: String) = {
        val bytes = plain.getBytes(Codec.UTF8.charSet)
        val md = MessageDigest.getInstance(algorithm)
        md.reset()
        md.update(salt.toArray)
        val hashed = (1 until n).foldLeft(md.digest(bytes)) { (bytes, i) =>
          md.reset(); md.digest(bytes)
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

  def apply = create(secretName(0), Bcrypt())_
  def apply(name: Ident) = create(name, Bcrypt())_
  def apply(crypt: Crypt) = create(secretName(0), crypt)_
  def apply(name: Ident, crypt: Crypt) = create(name, crypt)_
}

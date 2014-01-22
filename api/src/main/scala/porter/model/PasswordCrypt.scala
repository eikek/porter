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

package porter.model

import org.mindrot.jbcrypt.BCrypt
import scala.util.Try
import java.security.{MessageDigest, SecureRandom}
import java.util.concurrent.atomic.AtomicInteger
import com.lambdaworks.crypto.SCryptUtil
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import porter.util.Base64
import scala.io.Codec

/**
 * Creates an crypted string from a given plain text password.
 *
 * There are four provided crypt algorithms:
 *
 * - [[http://www.mindrot.org/projects/jBCrypt/ Bcrypt]]
 * - [[https://github.com/wg/scrypt Scrypt]]
 * - PBKDF2WithHmacSHA1 provided by JCE
 * - a message digest function like MD5 or SHA1/256/512 applied
 *   multiple times
 *
 * Each one can be configured manually or the provided default
 * arguments can be used. It is possible to create a [[porter.model.PasswordCrypt]]
 * from a configuration string. The string is expected to either be "random", which
 * will randomly choose one of the four crypt algorithms, or to start with the name
 * of the algorithm ("bcrypt", "scrypt", "pbkdf2-sha1" or "digest:algorithm", where
 * 'algorithm' is one of "MD5", "SHA-1", "SHA256" or "SHA-512") optionally followed
 * by configuration paramters specific for each algorithm separated by colons ':'.
 *
 * To verify passwords, a verifyer can be obtained from an crypted
 * password string for each algorithm using extractors
 * {{{
 *   encodedpassword match {
 *     case PasswordCrypt.Bcrypt.Verify(verify) => verify(plaintextpassword)
 *     case _ => false
 *   }
 * }}}
 * If the concrete algorithm used is not of interest, the `verify` function
 * can be used instead:
 * {{{
 *   PasswordCrypt.verify(cryptedpassword, plainpassword)
 * }}}
 */
trait PasswordCrypt extends (String => String)

object PasswordCrypt {
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

  type Verifier = String => Boolean
  import porter.util._

  def randomCrypt = intBelow(4) match {
    case 0 => Bcrypt()
    case 1 => Scrypt()
    case 2 => Pbkdf2()
    case 3 => Digest.sha512()
  }

  def apply(config: String): Option[PasswordCrypt] =
    if (config.toLowerCase == "random") Some(randomCrypt)
    else Bcrypt.fromConfig(config)
      .orElse(Scrypt.fromConfig(config))
      .orElse(Pbkdf2.fromConfig(config))
      .orElse(Digest.fromConfig(config))

  def unapply(encoded: String): Option[Verifier] =
    Bcrypt.Verify.unapply(encoded)
      .orElse(Scrypt.Verify.unapply(encoded))
      .orElse(Pbkdf2.Verify.unapply(encoded))
      .orElse(Digest.Verify.unapply(encoded))

  def verify(plain: String, crypted: String): Boolean = crypted match {
    case PasswordCrypt(verify) => verify(plain)
    case _ => false
  }

  object Bcrypt {
    val name = "bcrypt"
    def apply(): PasswordCrypt = apply(12)
    def apply(strength: Int): PasswordCrypt = apply(BCrypt.gensalt(strength))
    def apply(salt: String): PasswordCrypt = new PasswordCrypt {
      def apply(plain: String) = name +"$"+BCrypt.hashpw(plain, salt)
    }

    def fromConfig(config: String): Option[PasswordCrypt] = config match {
      case `name` => Some(apply())
      case n if n startsWith (name+":") =>
        val strength = Try(n.substring(name.length +1).toInt).toOption
        strength.map(apply)
      case _ => None
    }

    object Verify {
      def unapply(enc: String): Option[Verifier] =
        if (enc startsWith (name+"$")) Some((plain:String) => BCrypt.checkpw(plain, enc.substring(name.length+1)))
        else None
    }
  }
  object Scrypt {
    val name = "scrypt"
    def apply(n: Int = math.pow(2, 15).toInt, r: Int = 16, p: Int = 2): PasswordCrypt = new PasswordCrypt {
      def apply(plain: String) = name+"$"+SCryptUtil.scrypt(plain, n, r, p)
    }

    def fromConfig(config: String): Option[PasswordCrypt] = config match {
      case `name` => Some(apply())
      case str => split(str, ':') match {
        case `name`::n::Nil =>
          Try(n.toInt).toOption.map(apply(_))
        case `name`::sn::sr::Nil =>
          Try((sn.toInt, sr.toInt)).toOption.map { case (n, r) => apply(n, r) }
        case `name`::sn::sr::sp::Nil =>
          Try((sn.toInt, sr.toInt, sp.toInt)).toOption.map { case (n,r,p) => apply(n,r,p) }
        case _ => None
      }
    }

    object Verify {
      def unapply(enc: String): Option[Verifier] =
        if (enc startsWith (name+"$")) Some((plain: String) => SCryptUtil.check(plain, enc.substring(name.length+1)))
        else None
    }
  }

  object Pbkdf2 {
    val name = "pbkdf2-sha1"

    def apply(n: Int = 32000, len: Int = 512, salt: Vector[Byte] = randomSalt): PasswordCrypt = new PasswordCrypt {
      def apply(plain: String) = {
        val enc = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
          .generateSecret(new PBEKeySpec(plain.toCharArray, salt.toArray, n, len))
          .getEncoded
        name +"$"+n+"$"+len+"$"+ Base64.encode(salt) +"$"+Base64.encode(enc)
      }
    }

    def fromConfig(str: String): Option[PasswordCrypt] = str match {
      case `name` => Some(apply())
      case s => split(s, ':') match {
        case `name`::sn::len::Nil => Try((sn.toInt, len.toInt)).toOption.map({ case (n, l) => apply(n, l) })
        case `name`::sn::Nil => Try(sn.toInt).toOption.map(apply(_))
        case _ => None
      }
    }

    object Verify {
      def unapply(encoded: String):Option[Verifier] = split(encoded, '$') match {
        case `name`::iter::len::salt::data::Nil =>
          Try((iter.toInt, len.toInt, Base64.decode(salt).toVector)).map { case (n, l, s) =>
            (plain: String) => Pbkdf2(n, l, s)(plain) == encoded
          }.toOption
        case _ => None
      }
    }
  }

  object Digest {
    val name = "digest"
    def md5(n: Int = 500000, salt: Vector[Byte] = randomSalt): PasswordCrypt = apply("MD5", n, salt)
    def sha1(n: Int = 500000, salt: Vector[Byte] = randomSalt): PasswordCrypt = apply("SHA-1", n, salt)
    def sha256(n: Int = 500000, salt: Vector[Byte] = randomSalt): PasswordCrypt = apply("SHA-256", n, salt)
    def sha512(n: Int = 500000, salt: Vector[Byte] = randomSalt): PasswordCrypt = apply("SHA-512", n, salt)

    def apply(algorithm: String, n: Int = 500000, salt: Vector[Byte] = randomSalt): PasswordCrypt = new PasswordCrypt {
      def apply(plain: String) = {
        val bytes = plain.getBytes(Codec.UTF8.charSet)
        val md = MessageDigest.getInstance(algorithm.toUpperCase)
        md.reset()
        md.update(salt.toArray)
        val hashed = (1 until n).foldLeft(md.digest(bytes)) { (bytes, i) =>
          md.reset(); md.digest(bytes)
        }
        name+"$"+algorithm+"$"+n+"$"+Base64.encode(salt) +"$"+ Base64.encode(hashed)
      }
    }

    def fromConfig(str: String): Option[PasswordCrypt] = split(str, ':') match {
      case `name`::algo::Nil => Some(apply(algo))
      case `name`::algo::sn::Nil => Try(sn.toInt).toOption.map(apply(algo, _))
      case _ => None
    }

    object Verify {
      def unapply(encoded: String): Option[Verifier] = split(encoded, '$') match {
        case `name`::algo::iter::salt::data::Nil =>
          Try((algo, iter.toInt, Base64.decode(salt).toVector)).map { case (a, n, s) =>
            (plain: String) => Digest(a, n, s)(plain) == encoded
          }.toOption
        case _ => None
      }
    }
  }
}
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
  import Secret._

  type MatcherIn = (PasswordCredentials, Secret)
  type MatcherOut = (Secret, Vote)
  type Matcher = PartialFunction[MatcherIn, MatcherOut]

  private val matcher = (checkBCryptPassword orElse
    checkSCryptPassword orElse
    checkPBKDF2Password orElse
    checkShiroPassword).lift

  def authenticate(token: AuthToken) = {
    val userpass = token.credentials collect { case c: PasswordCredentials => c }
    val secrets = token.account.secrets
    userpass.foldLeft(token) { (token, up) =>
      secrets.foldLeft(token) { (token, sec) =>
        matcher((up, sec)).map(token.vote).getOrElse(token)
      }
    }
  }

  private def checkBCryptPassword: Matcher = {
    case (up: PasswordCredentials, secret) if secret.name == Types.bcrypt =>
      if (BCrypt.checkpw(up.password, secret.asString)) secret -> Vote.Success
      else secret -> Vote.Failed()
  }

  private def checkSCryptPassword: Matcher = {
    case (up: PasswordCredentials, secret) if secret.name == Types.scrypt =>
      if (SCryptUtil.check(up.password, secret.asString)) secret -> Vote.Success
      else secret -> Vote.Failed()
  }

  private def checkPBKDF2Password: Matcher = {
    case (up: PasswordCredentials, secret) if secret.name == Types.pbkdf =>
      val pass = secret.asString
      pass.split("\\$").toList.drop(1) match {
        case name::iter::salt::pw::Nil => {
          val tmp = Secret.pbkdf2PasswordWithParams(up.password, Base64.decode(salt).toVector, iter.toInt)
          if (tmp.asString == pass) secret -> Vote.Success
          else secret -> Vote.Failed()
        }
        case _ => secret -> Vote.Failed()
    }
  }

  private def checkShiroPassword: Matcher = {
    case (up: PasswordCredentials, secret) if secret.asString.startsWith("$shiro") =>
      val pass = secret.asString
      pass.split("\\$").toList.drop(1) match {
        case shiro::algorithm::iter::salt::data::Nil =>
          val encoded = createShiroHash(iter.toInt, algorithm, Base64.decode(salt).toArray, up.password)
          if (encoded == data) secret -> Vote.Success
          else secret -> Vote.Failed()
        case _ => secret -> Vote.Failed()
      }
  }

  private[auth] def createShiroHash(iter: Int, algorithm: String, salt: Array[Byte], pwplain: String): String = {
    val bytes = pwplain.getBytes(Codec.UTF8.charSet)
    val md = MessageDigest.getInstance(algorithm)
    md.reset()
    md.update(salt)
    var hashed = md.digest(bytes)
    for (i <- 1 until iter) {
      md.reset()
      hashed = md.digest(hashed)
    }
    Base64.encode(hashed)
  }
}

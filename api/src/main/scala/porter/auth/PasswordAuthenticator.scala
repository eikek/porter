package porter.auth


import org.mindrot.jbcrypt.BCrypt
import com.lambdaworks.crypto.SCryptUtil
import porter.util.Base64

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 22:07
 */
object PasswordAuthenticator extends Authenticator {
  import porter.model._
  import Secret._

  type MatcherIn = (PasswordCredentials, Secret)
  type MatcherOut = (Secret, Vote)
  type Matcher = PartialFunction[MatcherIn, MatcherOut]

  private val matcher = (checkBCryptPassword orElse
    checkSCryptPassword orElse
    checkPBKDF2Password).lift

  def authenticate(token: AuthToken) = {
    val userpass = token.credentials collect { case c: PasswordCredentials => c }
    userpass.headOption match {
      case Some(up) =>
        token.account.secrets.foldLeft(token) { (tok, sec) =>
          matcher((up, sec)).map(token.vote).getOrElse(token)
        }
      case _ => token
    }
  }

  private def checkBCryptPassword: Matcher = {
    case (up: PasswordCredentials, secret) if secret.name == Types.bcrypt => {
      if (BCrypt.checkpw(up.password, secret.asString)) secret -> Vote.Success
      else secret -> Vote.Failed()
    }
  }

  private def checkSCryptPassword: Matcher = {
    case (up: PasswordCredentials, secret) if secret.name == Types.scrypt => {
      if (SCryptUtil.check(up.password, secret.asString)) secret -> Vote.Success
      else secret -> Vote.Failed()
    }
  }

  private def checkPBKDF2Password: Matcher = {
    case (up: PasswordCredentials, secret) if secret.name == Types.pbkdf => {
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
  }

}

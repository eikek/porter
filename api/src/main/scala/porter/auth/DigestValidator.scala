package porter.auth

import porter.util.Hash
import scala.util.{Failure, Success, Try}

/**
 * @since 30.11.13 13:21
 */
object DigestValidator extends Validator {

  import porter.model._
  import scala.concurrent.duration._

  object Reasons {
    val nonceExpired = Vote.Failed(Ident("NonceExpired") -> "Nonce expired.")
    val invalidCred = Vote.Failed(Ident("InvalidCredentials") -> "Invalid credentials")
  }

  val secretString = java.util.UUID.randomUUID().toString

  /**
   * Generates a string to be used as the server side nonce value. It is comprised
   * of the timestamp and a checksum of a secret string + timestamp.
   *
   * If a digest authentication is requested from the client you must use a nonce value
   * generated from this method, if you want to use this authenticator. This authenticator
   * will check the nonce value send by the client and rejects the authentication request,
   * if that fails.
   *
   * @return
   */
  def generateNonce(timeout: Duration = 2.minutes) = createNonce(System.currentTimeMillis() + timeout.toMillis)

  private def createNonce(timestamp: Long) =
    timestamp + "$" + PasswordCrypt.Bcrypt(10)(timestamp + secretString)

  def authenticate(token: AuthToken) = {
    val credentials = token.credentials.collect({ case c: DigestCredentials => c })
    val secrets = token.account.secrets.filter(s => s.name.name.startsWith("digestmd5."))
    credentials.foldLeft(token) { (token, cred) =>
      secrets.foldLeft(token) { (token, sec) =>
        val vote = checkNonce(cred) match {
          case Success(ts) if !timedout(ts) => checkDigest(cred, sec.asString)
          case Success(ts) if timedout(ts) => Reasons.nonceExpired
          case Failure(ex) => Reasons.invalidCred
        }
        token.vote(sec -> vote)
      }
    }
  }

  private def timedout(ts: Long) = System.currentTimeMillis() > ts

  private def checkDigest(dc: DigestCredentials, ha1: String): Vote = {
    val ha2 = dc.qop match {
      case Some(DigestAuthInt(_, _, body)) => Hash.md5String(dc.method +":"+ dc.uri +":"+ body)
      case _ => Hash.md5String(dc.method +":"+ dc.uri)
    }
    val serverResp = dc.qop match {
      case Some(qop) => Hash.md5String(ha1 +":"+ dc.serverNonce +":"+ qop.nonceCount +":"+ qop.cnonce +":"+ qop.name +":"+ ha2)
      case _ => Hash.md5String(ha1 +":"+ dc.serverNonce +":"+ ha2)
    }
    if (serverResp == dc.response) Vote.Success else Reasons.invalidCred
  }

  def checkNonce(creds: DigestCredentials): Try[Long] = Try {
    val (clientTs, hash) = creds.serverNonce.indexOf('$') match {
      case idx if idx > 0 => (creds.serverNonce.substring(0, idx).toLong, creds.serverNonce.substring(idx+1))
      case _ => sys.error("Invalid nonce")
    }
    if (PasswordCrypt.verify(clientTs + secretString, hash)) clientTs
    else sys.error("Invalid nonce")
  }
}

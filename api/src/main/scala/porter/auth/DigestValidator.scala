package porter.auth

import porter.util.Hash
import scala.util.{Failure, Success, Try}

object DigestValidator extends Validator {

  import porter.model._

  object Reasons {
    val nonceExpired = Vote.Failed(Ident("NonceExpired") -> "Nonce expired.")
    val invalidCred = Vote.Failed(Ident("InvalidCredentials") -> "Invalid credentials")
  }

  def authenticate(token: AuthToken) = {
    val credentials = token.credentials.collect({ case c: DigestCredentials => c })
    val secrets = token.account.secrets.filter(s => s.name.name.startsWith("digestmd5."))
    credentials.foldLeft(token) { (token, cred) =>
      secrets.foldLeft(token) { (token, sec) =>
        val vote = Nonce.extractTimestamp(cred.serverNonce) match {
          case Success(ts) if Nonce.timestampValid(ts) => checkDigest(cred, sec.asString)
          case Success(ts) => Reasons.nonceExpired
          case Failure(ex) => Reasons.invalidCred
        }
        token.vote(sec -> vote)
      }
    }
  }

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
}

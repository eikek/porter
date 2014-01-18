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

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

package porter.app.client.spray

import spray.http.{GenericHttpCredentials, HttpRequest, HttpCredentials}
import porter.model.{DigestAuth, DigestAuthInt, DigestCredentials}
import porter.util.Hash

object HttpDigestCredentials {
  def unapply(in: (HttpCredentials, HttpRequest)): Option[DigestCredentials] = {
    val (creds, req) = in
    val method = req.method.value
    lazy val body = Hash.md5String(req.entity.asString)

    creds match {
      case GenericHttpCredentials("Digest", _, params) => {
        (params.get("username"),
          params.get("response"),
          params.get("realm"),
          params.get("uri"),
          params.get("nonce"),
          params.get("cnonce"),
          params.get("qop"),
          params.get("nc")) match {

          case (Some(un), Some(resp), Some(realm), Some(uri), Some(nonce), cnonce, qop, nc) => {
            (cnonce, qop, nc) match {
              case (Some(cn), Some(q), Some(n)) if q == "auth-int" =>
                Some(DigestCredentials(un, method, uri, nonce, resp, Some(DigestAuthInt(cn, n, body))))
              case (Some(cn), Some(q), Some(n)) if q == "auth" =>
                Some(DigestCredentials(un, method, uri, nonce, resp, Some(DigestAuth(cn, n))))
              case _ => Some(DigestCredentials(un, method, uri, nonce, resp, None))
            }
          }
          case _ => None
        }
      }
      case _ => None
    }
  }
}

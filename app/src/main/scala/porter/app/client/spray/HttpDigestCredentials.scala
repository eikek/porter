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

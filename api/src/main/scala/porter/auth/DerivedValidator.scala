package porter.auth

import porter.model.DerivedCredentials
import porter.util.Hash

/**
 * @since 01.12.13 17:05
 */
object DerivedValidator extends Validator {

  def authenticate(token: AuthToken) = {
    val derived = token.credentials.collect { case dc: DerivedCredentials => dc }
    val found = for {
      cred <- derived
      accsecr <- token.account.secrets.find(s => s.name == cred.secret.name)
    } yield accsecr -> (!cred.isExpired && cred.secret.data == Hash.sha512(accsecr.data))
    found.foldLeft(token) { case (t, (s, bool)) =>
      if (bool) t vote (s -> Vote.Success)
      else t vote (s -> Vote.Failed())
    }
  }
}

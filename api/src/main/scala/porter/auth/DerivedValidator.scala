package porter.auth

import porter.model.DerivedCredentials

/**
 * @since 01.12.13 17:05
 */
object DerivedValidator extends Validator {

  def authenticate(token: AuthToken) = {
    val derived = token.credentials.collect { case dc: DerivedCredentials => dc }
    val found = for {
      cred <- derived.headOption
      secr <- cred.secret.toOption
      accsecr <- token.account.secrets.find(s => s.name == secr.name)
    } yield accsecr -> (secr.data == accsecr.data)
    found match {
      case Some((s, true)) => token vote s -> Vote.Success
      case Some((s, false)) => token vote s -> Vote.Failed()
      case _ => token
    }
  }
}

package porter.auth

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 19:27
 */
trait Authenticator {

  def authenticate(token: AuthToken): AuthToken

}

trait AuthenticatorProvider {

  def authenticators: Iterable[Authenticator]

}
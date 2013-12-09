package porter.auth

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 19:27
 */
trait Validator {

  def authenticate(token: AuthToken): AuthToken

}

trait ValidatorProvider {

  def validators: Iterable[Validator]

}
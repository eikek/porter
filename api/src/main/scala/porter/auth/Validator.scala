package porter.auth

trait Validator {

  def authenticate(token: AuthToken): AuthToken

}

trait ValidatorProvider {

  def validators: Iterable[Validator] = List(PasswordValidator, DerivedValidator, DigestValidator)

}
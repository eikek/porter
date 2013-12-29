package porter.auth

/**
 * @since 22.11.13 22:07
 */
object PasswordValidator extends Validator {
  import porter.model._
  import Password._

  def authenticate(token: AuthToken) = {
    val userpass = token.credentials collect { case c: PasswordCredentials => c }
    val secrets = token.account.secrets.filter(_.name.name.startsWith("password."))
    userpass.foldLeft(token) { (token, up) =>
      secrets.foldLeft(token) { (token, sec) =>
        if (verifyPassword(up.password, sec.asString)) token.vote(sec -> Vote.Success)
        else token.vote(sec -> Vote.Failed())
      }
    }
  }

  @scala.annotation.tailrec
  private def verifyPassword(plain: String, hashed: String): Boolean =
    if (hashed startsWith "$shiro1$") verifyPassword(plain, hashed.replace("$shiro1$", "digest$"))
    else Password.verify(plain, hashed)

}

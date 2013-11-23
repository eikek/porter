package porter.auth

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.mindrot.jbcrypt.BCrypt

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 22:53
 */
class PasswordAuthenticatorTest extends FunSuite with ShouldMatchers {

  import porter.model._
  import Secret._

  private def createToken(pw: Secret, plain: String) = AuthToken(
    Realm(Ident.randomIdent, ""),
    Account(name = "john", secrets = Seq(pw)),
    Set(new PasswordCredentials("john", plain))
  )


  test("check password match") {
    checkpasswords(Types.bcrypt, Secret.bcryptPassword(_, BCrypt.gensalt(10)))
    checkpasswords(Types.scrypt, Secret.scryptPassword(_, math.pow(2, 10).toInt, 16, 2))
    checkpasswords(Types.pbkdf, Secret.pbkdf2Password)
  }

  private def checkpasswords(t: Ident, f: String => Secret) {
    var token = createToken(f("helpme"), "helpme")
    var out = PasswordAuthenticator.authenticate(token)
    out.votes(t) should be (Vote.Success)

    token = createToken(f("nono"), "nono2")
    out = PasswordAuthenticator.authenticate(token)
    out.votes(t) should be (Vote.Failed())

    token = createToken(Secret("unknown", ""), "test")
    out = PasswordAuthenticator.authenticate(token)
    out.votes should have size 0
  }
}

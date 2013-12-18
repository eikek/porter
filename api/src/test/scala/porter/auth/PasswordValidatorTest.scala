package porter.auth

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.mindrot.jbcrypt.BCrypt
import porter.util.Base64

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 22:53
 */
class PasswordValidatorTest extends FunSuite with ShouldMatchers {

  import porter.model._
  import Secret._

  private def createToken(pw: Secret, plain: String) = AuthToken(
    Realm(Ident.randomIdent, ""),
    Account(name = "john", secrets = Seq(pw)),
    Set(PasswordCredentials("john", plain))
  )


  test("check password match") {
    checkpasswords(Types.bcrypt, Secret.bcryptPassword(_, BCrypt.gensalt(10)))
    checkpasswords(Types.scrypt, Secret.scryptPassword(_, math.pow(2, 10).toInt, 16, 2))
    checkpasswords(Types.pbkdf, Secret.pbkdf2Password)
  }

  test("check shiro passwords") {
    val salt = Array(123.toByte, 23.toByte)
    val cryptFun: String => Secret = plain => {
      val data = PasswordValidator.createShiroHash(100000, "SHA-512", salt, plain)
      Secret("shiro-pass", "$shiro1$SHA-512$100000$"+Base64.encode(salt)+"$"+data)
    }
    checkpasswords("shiro-pass", cryptFun)

    //created using shiro 1.2.1
    val shiroSecret = Secret("shiro-password", ("$shiro1$SHA-512" +
      "$500000$Teyvy3etXrD2T29KV3v95w==" +
      "$KD08pQGtoCVetsQ67lyIf6DYOTNpnH4wu241X+RzWkYUYFxYcqXITdcbawaGBTM7q7206cQGpni86fhhkS6gRg==").getBytes)
    val token = createToken(shiroSecret, "Fc8iFbp4o7JyC5TaXmkjAJK9n")
    val out = PasswordValidator.authenticate(token)
    out.votes("shiro-password") should be (Vote.Success)
  }

  private def checkpasswords(t: Ident, f: String => Secret) {
    var token = createToken(f("helpme"), "helpme")
    var out = PasswordValidator.authenticate(token)
    out.votes(t) should be (Vote.Success)

    token = createToken(f("nono"), "nono2")
    out = PasswordValidator.authenticate(token)
    out.votes(t) should be (Vote.Failed())

    token = createToken(Secret("unknown", ""), "test")
    out = PasswordValidator.authenticate(token)
    out.votes should have size 0
  }
}

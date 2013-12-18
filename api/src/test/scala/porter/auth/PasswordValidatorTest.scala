package porter.auth

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.util.Base64

class PasswordValidatorTest extends FunSuite with ShouldMatchers {

  import porter.model._
  import Password._

  private def createToken(pw: Secret, plain: String) = AuthToken(
    Realm(Ident.randomIdent, ""),
    Account(name = "john", secrets = Seq(pw)),
    Set(PasswordCredentials("john", plain))
  )


  test("check password match") {
    checkpasswords("bcrypt", "password.0", Password(Bcrypt(10)))
    checkpasswords("scrypt", "password.0", Password(Scrypt()))
    checkpasswords("pbkdf2", "password.0", Password(Pbkdf2()))
    checkpasswords("sha1", "password.0", Password(Digest.sha1()))
    checkpasswords("md5", "password.0", Password(Digest.md5()))
    checkpasswords("sha512", "password.0", Password(Digest.sha512()))
  }

  test("check shiro passwords") {
    //created using shiro 1.2.1
    val shiroSecret = Secret("password.0", "$shiro1$SHA-512" +
      "$500000$Teyvy3etXrD2T29KV3v95w==" +
      "$KD08pQGtoCVetsQ67lyIf6DYOTNpnH4wu241X+RzWkYUYFxYcqXITdcbawaGBTM7q7206cQGpni86fhhkS6gRg==")
    val token = createToken(shiroSecret, "Fc8iFbp4o7JyC5TaXmkjAJK9n")
    val out = PasswordValidator.authenticate(token)
    out.votes("password.0") should be (Vote.Success)
  }

  private def checkpasswords(name: String, t: Ident, f: String => Secret) {
    var token = createToken(f("helpme"), "helpme")
    var out = PasswordValidator.authenticate(token)
    assert(out.votes(t) === Vote.Success, name)

    token = createToken(f("nono"), "nono2")
    out = PasswordValidator.authenticate(token)
    assert(out.votes(t) === Vote.Failed(), name)

    token = createToken(Secret("unknown", ""), "test")
    out = PasswordValidator.authenticate(token)
    assert(out.votes.size === 0, name)
  }
}

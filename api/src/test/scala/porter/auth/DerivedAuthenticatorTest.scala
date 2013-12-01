package porter.auth

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.model._
import porter.util.AES
import porter.model.Realm
import porter.model.Account
import porter.model.Secret.Types

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 01.12.13 17:36
 */
class DerivedAuthenticatorTest extends FunSuite with ShouldMatchers {

  val realm = Realm(Ident.randomIdent, "")
  val aeskey = AES.deriveKey("superword", Vector(81.toByte))
  val secret = Secret.bcryptPassword("test")

  test("authenticate") {
    val derived = DerivedCredentials("john", secret)(aeskey)
    val account = Account(name = "john", secrets = Seq(secret))
    val token = AuthToken(realm, account, Set(derived))
    val result = DerivedAuthenticator.authenticate(token)
    result.votes should have size 1
    result.votes.get(Secret.Types.bcrypt).get should be (Vote.Success)
  }

  test("authenticate raw") {
    val data = "vIQ/nlQWixTDxybhqvxPpZH1203aMFSYjByPKMWSj3ilt9bUmeO5HixlI0pR39/F0RJ" +
      "aV1pFyOz5aXIfi6bMdqLqYCqnIeTQvgCQPzP10n0fZjyy2VL7G4N+jWiG50/WcJZH6m+MA9sTIQr" +
      "T3FOS/JKrT+0N8f6jkx0a59cQ5g0="
    val derived = DerivedCredentials(aeskey, data)

    val secretbytes = "$2a$14$XsRXwJkYcoK/.ypLhhbfnu1gcUT4tjYbkuMoZHNiEGhkT9HW4qz1m"
    val secret = Secret(Types.bcrypt, secretbytes.getBytes)

    val account = Account(name = "john", secrets = Seq(secret))
    val token = AuthToken(realm, account, Set(derived))
    val result = DerivedAuthenticator.authenticate(token)
    result.votes should have size 1
    result.votes.get(Secret.Types.bcrypt).get should be (Vote.Success)
  }
}

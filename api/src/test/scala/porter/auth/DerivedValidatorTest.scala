package porter.auth

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.model._
import porter.util.AES
import porter.model.Realm
import porter.model.Account

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 01.12.13 17:36
 */
class DerivedValidatorTest extends FunSuite with ShouldMatchers {

  val realm = Realm(Ident.randomIdent, "")
  val aeskey = AES.deriveKey("superword", Vector(81.toByte))
  val secret = Password("test")

  test("authenticate") {
    val derived = DerivedCredentials("john", secret)
    val account = Account(name = "john", secrets = Seq(secret))
    val token = AuthToken(realm, account, Set(derived))
    val result = DerivedValidator.authenticate(token)
    result.votes should have size 1
    result.votes.get(secret.name).get should be (Vote.Success)
  }

  test("authenticate raw") {
    val data = DerivedCredentials("john", secret).encode(aeskey)
    val derived = DerivedCredentials.tryDecode(aeskey, data).get
    val account = Account(name = "john", secrets = Seq(secret))
    val token = AuthToken(realm, account, Set(derived))
    val result = DerivedValidator.authenticate(token)
    result.votes should have size 1
    result.votes.get(secret.name).get should be (Vote.Success)
  }
}

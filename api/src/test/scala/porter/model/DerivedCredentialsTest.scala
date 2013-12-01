package porter.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import porter.util.AES

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 01.12.13 16:41
 */
class DerivedCredentialsTest extends FunSuite with ShouldMatchers {

  def genSalt = {
    val rand = new Random
    val bytes = Array.fill(32)(0.toByte)
    rand.nextBytes(bytes)
    bytes.toVector
  }

  test("encrypt and decrypt secrets") {
    val salt = genSalt
    val key = AES.deriveKey("superword", salt)

    val secret = Secret.scryptPassword("hello")
    val derived = DerivedCredentials("john", secret)(key)
    derived.account.get should be (Ident("john"))
    derived.secret.get should be (secret)
    derived.expires.get should be (-1L)
    derived.isExpired should be (false)
  }
}

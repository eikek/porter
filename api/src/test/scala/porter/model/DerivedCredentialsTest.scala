package porter.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import porter.util.{Hash, AES}

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
    val data = DerivedCredentials("john", secret).encode(key)
    val derived = DerivedCredentials.tryDecode(key, data).get
    derived.accountName should be (Ident("john"))
    derived.secret.data should be (Hash.sha512(secret.data))
    derived.secret.name should be (secret.name)
    derived.valid should be (-1L)
    derived.isExpired should be (false)
  }
}

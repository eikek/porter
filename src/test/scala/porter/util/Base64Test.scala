package porter.util

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.io.Codec

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 20.11.13 23:40
 */
class Base64Test extends FunSuite with ShouldMatchers {

  test("encode nothing") {
    Base64.encode(Seq.empty) should be("")
    Base64.decode("") should be(Seq.empty)
  }

  test("encode(Hello World)") {
    Base64.encode("Hello World".getBytes(Codec.UTF8.name)) should be("SGVsbG8gV29ybGQ=")
  }

  test("decode with whitespace") {
    val str = "This is a string"
    val enc = Base64.encode(str.getBytes(Codec.UTF8.name))

    List("\n", "\r", "\t") foreach {
      w =>
        val enc2 = enc.flatMap(c => c + w)
        Base64.decode(enc2).toArray should be(str.getBytes(Codec.UTF8.name))
    }
  }

  test("ignore non base64 characters") {
    val enc = Base64.encode("helloworld".getBytes)
    val bad = enc.mkString("$")
    Base64.decode(bad).toArray should be ("helloworld".getBytes)
  }

  test("random encode/decode") {
    for (i <- 1 to 100) {
      val len = (math.random * 120).toInt
      val bytes = Seq.fill(len)((math.random*255).toByte)
      val enc = Base64.encode(bytes)
      val dec = Base64.decode(enc)
      dec should be (bytes)
    }
  }

  test("rfc4648 vectors") {
    Base64.decode("Zg==").toArray should be ("f".getBytes)
    Base64.decode("Zm8=").toArray should be ("fo".getBytes)
    Base64.decode("Zm9v").toArray should be ("foo".getBytes)
    Base64.decode("Zm9vYg==").toArray should be ("foob".getBytes)
    Base64.decode("Zm9vYmE=").toArray should be ("fooba".getBytes)
    Base64.decode("Zm9vYmFy").toArray should be ("foobar".getBytes)

    Base64.encode("f".getBytes) should be ("Zg==")
    Base64.encode("fo".getBytes) should be ("Zm8=")
    Base64.encode("foo".getBytes) should be ("Zm9v")
    Base64.encode("foob".getBytes) should be ("Zm9vYg==")
    Base64.encode("fooba".getBytes) should be ("Zm9vYmE=")
    Base64.encode("foobar".getBytes) should be ("Zm9vYmFy")
  }
}


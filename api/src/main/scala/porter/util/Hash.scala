package porter.util

import java.security.MessageDigest
import scala.io.Codec
import javax.xml.bind.DatatypeConverter

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 20.11.13 22:28
 */
object Hash {

  val md5 = stringDigest("MD5")_
  val sha1 = stringDigest("SHA1")_
  val sha256 = stringDigest("SHA-256")_
  val sha384 = stringDigest("SHA-384")_
  val sha512 = stringDigest("SHA-512")_

  def stringDigest(algorithm: String)(str: String) = {
    val md = MessageDigest.getInstance(algorithm)
    md.update(str.getBytes(Codec.UTF8.charSet))
    DatatypeConverter.printHexBinary(md.digest()).toLowerCase
  }
}

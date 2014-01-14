package porter.util

import java.security.MessageDigest
import scala.io.Codec
import javax.xml.bind.DatatypeConverter

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 20.11.13 22:28
 */
object Hash {

  val md5 = digest("MD5")_
  val sha1 = digest("SHA1")_
  val sha256 = digest("SHA-256")_
  val sha384 = digest("SHA-384")_
  val sha512 = digest("SHA-512")_

  val md5String = stringDigest(md5)_
  val sha1String = stringDigest(sha1)_
  val sha256String = stringDigest(sha256)_
  val sha384String = stringDigest(sha384)_
  val sha512String = stringDigest(sha512)_

  private def stringDigest(digest: Vector[Byte] => Vector[Byte])(str: String) = {
    val bytes = digest(str.getBytes(Codec.UTF8.charSet).toVector)
    toHexBinary(bytes)
  }
  
  def digest(algorithm: String)(message: Vector[Byte]) = {
    val md = MessageDigest.getInstance(algorithm)
    md.update(message.toArray)
    md.digest().toVector
  }
  def toHexBinary(bytes: Seq[Byte]): String =
    DatatypeConverter.printHexBinary(bytes.toArray).toLowerCase
}

/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porter.util

import java.security.MessageDigest
import scala.io.Codec
import javax.xml.bind.DatatypeConverter

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

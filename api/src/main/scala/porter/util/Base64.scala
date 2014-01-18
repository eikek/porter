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

object Base64 {
  private val chars = ('A' to 'Z').toList ::: ('a' to 'z').toList ::: ('0' to '9').toList ::: List('+', '/')
  private val alphabet = ('=' :: chars).toSet

  def encodeChars(bytes: Seq[Byte]) =
    bytes.flatMap(toBinary).grouped(6) flatMap encodeGroup

  def encode(bytes: Seq[Byte]) = {
    val b = new StringBuilder
    encodeChars(bytes) foreach b.append
    b.toString()
  }

  def decodeChars(chars: Seq[Char]) =
    chars.filter(alphabet.contains).grouped(4) flatMap decodeGroup

  def decode(in: String): Seq[Byte] = decodeChars(in.toStream).toSeq

  private def encodeGroup(group: Seq[Int]) = group match {
    case g if g.size == 6 => Seq(chars(toDecimal(g)))
    case g if g.size == 4 => Seq(chars(toDecimal(g++Seq(0, 0))), '=')
    case g if g.size == 2 => Seq(chars(toDecimal(g ++ Seq(0, 0, 0, 0))), '=', '=')
    case _ => sys.error("group must be of size 6, 4 or 2 ")
  }

  private def decodeGroup(group: Seq[Char]): Seq[Byte] = {
    def decodeFour(s: Seq[Char]) = s.map(c => chars.indexOf(c))
      .flatMap(i => toBinary(i.toByte).drop(2))
      .grouped(8).map(g => toDecimal(g).toByte).toSeq

    group.toList match {
      case g if g(2) == '=' || g.length == 2 => decodeFour(Seq(g(0), g(1), 'A', 'A')).take(1)
      case g if g(3) == '=' || g.length == 3 => decodeFour(Seq(g(0), g(1), g(2), 'A')).take(2)
      case g => decodeFour(g)
    }
  }

  def toDecimal(bits: Seq[Int]) = {
    bits.zipWithIndex map { case (digit, i) =>
      val p = bits.size - i -1
      digit * math.pow(2, p).toInt
    } reduce(_ + _)
  }

  def toBinary(b: Byte) = {
    val bits = Array.fill(8)(0)
    for (i <- 0 to 7; if (b & 1 << i) != 0) {
      bits(bits.length -i -1) = 1
    }
    bits
  }
}

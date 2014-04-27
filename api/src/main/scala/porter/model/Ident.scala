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

package porter.model

import scala.language.implicitConversions
import scala.annotation.tailrec
import java.util.UUID

/**
  * An identifier is a string consisting of only letters (a-z and A-Z)
  * digits (0-9) the underscore '_' and dash '-'. All other characters
  * are not allowed.
  */
sealed trait Ident extends Serializable {
  def name: String
  def is(otherName:String) = name == otherName
}

object Ident {
  private val chars = ('a' to 'z').toSet ++ ('A' to 'Z') ++ ('0' to '9') ++ Set('_', '-', '@', '.', ':', '/')

  @SerialVersionUID(20131121)
  private case class Impl(name: String) extends Ident {
    override lazy val toString = s"Ident($name)"
  }

  implicit def apply(name: String): Ident = {
    require(isValid(name), s"Invalid identifier: '$name'")
    Impl(name)
  }

  def unapply(s: Ident): Option[String] = Some(s.name)

  /**
   * Checks whether the given string is a valid identifier.
   *
   * @param s
   * @return
   */
  def isValid(s: String) = {
    @tailrec def loop(i: Int): Boolean = {
      if (i >= s.length) i > 0
      else
        if (chars contains s.charAt(i)) loop(i+1)
        else false
    }
    loop(0)
  }

  /**
   * Creates an [[porter.model.Ident]] from the given string. If the
   * string is not a valid identifier, [[scala.None]] is returned.
   *
   * @param id
   * @return
   */
  def fromString(id: String): Option[Ident] = if (isValid(id)) Some(Impl(id)) else None

  /**
   * Converts the given string to a valid identifier by removing all
   * invalid characters from the string. If the resulting string is
   * empty, [[scala.None]] is returned, otherwise the [[porter.model.Ident]]
   * is created from the resulting string.
   *
   * @param s
   * @return
   */
  def convertString(s: String): Option[Ident] = {
    val conv = s filter chars.contains
    if (conv.isEmpty) None else Some(Impl(conv))
  }

  /**
   * Creates some random identifier.
   * @return
   */
  def randomIdent = Ident(UUID.randomUUID().toString.replace("-", ""))
}

object ValidIdent {
  def unapply(s: String) = Ident.fromString(s) map (_.name)
}
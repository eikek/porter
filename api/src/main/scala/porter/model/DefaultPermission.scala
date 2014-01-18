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

@SerialVersionUID(20131121)
final case class DefaultPermission(parts: Parts) extends Permission {

  def implies(other: Permission) = other match {
    case dp: DefaultPermission =>
      val compareParts = (parts zip dp.parts) forall {
        case (mp, op) => implies(mp, op)
      }
      if (!compareParts || parts.length <= dp.parts.length) compareParts && !parts.isEmpty
      else parts.drop(dp.parts.length) forall (_ == Set("*"))
    case _ => false
  }

  private def implies(a: Set[String], b: Set[String]): Boolean = {
    a == Set("*") || b.subsetOf(a)
  }

  override def toString = parts.map(gl => gl.mkString(",")).mkString(":")
}

object DefaultPermission {

  val factory: PermissionFactory = {
    case str => DefaultPermission(str)
  }

  def apply(str: String): DefaultPermission = {
    new DefaultPermission(split(str))
  }

  def split(str: String): Parts = {
    def splitter(chars: List[Char], sep: Char): List[String] =
      chars.foldRight(List("")) { (c, list) =>
        if (c == sep) "" :: list
        else (list.head + c) :: list.tail
      }

    val parts = splitter(str.toList, ':')
    if (parts.exists(_.isEmpty)) {
      throw new IllegalArgumentException(s"Invalid permission string: '$str'")
    }
    parts map { s =>
      splitter(s.toList, ',').filter(_.nonEmpty).toSet
    }
  }
}

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

import scala.annotation.tailrec

object Glob {
  type Path = List[String]

  def fromString(s: String): Path = s.split('/').filterNot(_.isEmpty).toList

  def matches(path: String, in: String) = matchesPath(fromString(path), fromString(in))

  @tailrec
  def matchesPath(path: Path, in: Path): Boolean = {
    (path.headOption, in.headOption) match {
      case (Some(a), None) if a == "**" =>
        if (in.isEmpty) path.tail.isEmpty else false

      case (Some(a), Some(b)) if a == "**" =>
        if (path.tail.isEmpty) true
        else {
          val next = path.tail.head
          //skip input until one matches the `next`
          val nextin = in.dropWhile(s => !simpleMatch(Some(next), Some(s)))
          matchesPath(path.tail, nextin)
        }

      case (a, b) => simpleMatch(a, b) match {
        case true => (path, in) match {
          case (Nil, Nil) => true
          case (p, input) if p.nonEmpty && input.nonEmpty => matchesPath(p.tail, input.tail)
          case (p::Nil, Nil) if p == "*" || p == "**" => true
          case _ => false
        }
        case false => false
      }
    }
  }

  private def simpleMatch(x: Option[String], y: Option[String]) = {
    (x, y) match {
      case (None, None) => true
      case (Some(a), Some(b)) if a == b => true
      case (Some(a), Some(b)) if a.contains("?") =>
        (true /: a.zip(b)) { (b, c) => b &&  c._1 == '?' || c._1 == c._2 }
      case (Some(a), _) if a == "*" => true
      case (Some(a), Some(b)) if a.startsWith("*") => b.endsWith(a.drop(1))
      case (Some(a), Some(b)) if a.endsWith("*") => b.startsWith(a.dropRight(1))
      case _ => false
    }
  }
}

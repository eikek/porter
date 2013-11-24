package porter.util

import scala.annotation.tailrec

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 22:10
 */
object Glob {
  type Path = List[String]

  def fromString(s: String) = s.split('/').filterNot(_.isEmpty).toList

  def matches(path: String, in: String) = matchesPath(fromString(path), fromString(in))

  @tailrec
  def matchesPath(path: Path, in: Path): Boolean = {
    (path.headOption, in.headOption) match {
      case (Some(a), None) if a == "**" => {
        if (in.isEmpty) path.tail.isEmpty else false
      }
      case (Some(a), Some(b)) if a == "**" => {
        if (path.tail.isEmpty) true
        else {
          val next = path.tail.head
          //skip input until one matches the `next`
          val nextin = in.dropWhile(s => !simpleMatch(Some(next), Some(s)))
          matchesPath(path.tail, nextin)
        }
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
      case (Some(a), Some(b)) if a.contains("?") => {
        (true /: a.zip(b)) { (b, c) => b &&  c._1 == '?' || c._1 == c._2 }
      }
      case (Some(a), _) if a == "*" => true
      case (Some(a), Some(b)) if a.startsWith("*") => b.endsWith(a.drop(1))
      case (Some(a), Some(b)) if a.endsWith("*") => b.startsWith(a.dropRight(1))
      case _ => false
    }
  }
}

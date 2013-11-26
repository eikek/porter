package porter.model

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 21:12
 */
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

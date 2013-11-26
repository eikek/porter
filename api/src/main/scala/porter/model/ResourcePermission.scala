package porter.model

import porter.util.Glob

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 22:08
 */
@SerialVersionUID(20131121)
final case class ResourcePermission(parts: Parts) extends Permission {
  require(parts.size == 3, "Invalid resource permission: "+ parts)

  val actions = parts.tail.head
  val paths = parts.tail.tail.head.toList.map(Glob.fromString)

  def implies(other: Permission) = other match {
    case rp: ResourcePermission =>
      actions == Set("*") || rp.actions.subsetOf(actions) && implyPaths(rp.paths)
    case _ => false
  }

  private def implyPaths(other: List[List[String]]) =
    other.forall(op => paths.exists(tp => Glob.matchesPath(tp, op)))

  override def toString = parts.map(gl => gl.mkString(",")).mkString(":")
}

object ResourcePermission {

  private val prefix = "resource:"

  val factory: PermissionFactory = {
    case str if str startsWith prefix => ResourcePermission(str)
  }

  def apply(str: String): ResourcePermission = {
    if (str.isEmpty) {
      throw new IllegalArgumentException("Empty permission strings not allowed")
    }
    val perm = if (str startsWith prefix) str else prefix+str
    val parts = DefaultPermission.split(perm)
    ResourcePermission(parts)
  }
}
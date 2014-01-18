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

import porter.util.Glob

@SerialVersionUID(20131121)
final case class ResourcePermission(parts: Parts) extends Permission {
  require(parts.size == 3, "Invalid resource permission: "+ parts)

  val actions = parts.tail.head
  val paths = parts.tail.tail.head.toList.map(Glob.fromString)

  def implies(other: Permission) = other match {
    case rp: ResourcePermission =>
      (actions == Set("*") || rp.actions.subsetOf(actions)) && implyPaths(rp.paths)
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
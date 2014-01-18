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

trait Permission extends Serializable {

  def implies(other: Permission): Boolean

  def revoke = Revocation(this)
}

object Permission {

  val allPermission: Permission = new Permission {
    def implies(other: Permission) = true
    override def toString = "porter.AllPermission"
  }

  val allPermissionFactory: PermissionFactory = {
    case perms if perms == allPermission.toString => allPermission
  }

  def apply(str: String) = DefaultPermission(str)

  /**
   * Returns true if all permissions in "check" are covered by the permissions
   * in "given".
   *
   *@param given
   * @param check
   * @return
   */
  def impliesAll(given: Iterable[Permission], check: Iterable[Permission]): Boolean =
    check.forall(pb => given.exists(pa => pa implies pb))

  /**
   * Returns true if at least one of the permissions in "check" is covered
   * by the permissions in "given".
   *
   * @param given
   * @param check
   * @return
   */
  def impliesOne(given: Iterable[Permission], check: Iterable[Permission]): Boolean =
    check.exists(pb => given.exists(pa => pa implies pb))

  /**
   * Reduces the list of permissions by removing those that are
   * implied by others in the list.
   *
   * @param perms
   * @return
   */
  def reduce(perms: Iterable[Permission]): Set[Permission] = {
    perms.foldLeft(perms.toSet) { (set, p) =>
      val rest = set.filterNot(_ == p)
      if (impliesAll(rest, List(p))) rest else set
    }
  }

  def diff(current: Set[Permission], remove: Set[Permission]): Set[Permission] = {
    remove.foldLeft(current) { (set, r) =>
      set.filterNot(p => impliesAll(remove, Set(p)))
    }
  }

  def union(one: Set[Permission], two: Set[Permission]): Set[Permission] = {
    reduce(one ++ two)
  }
}
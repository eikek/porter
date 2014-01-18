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

final case class Revocation(perm: Permission) {
  def revokes(p: Permission) = perm implies p
  override def toString = "!"+perm.toString
}

object Revocation {

  def revokesAll(given: Iterable[Revocation], check: Iterable[Permission]): Boolean =
    Permission.impliesAll(given.map(_.perm), check)

  def revokesOne(given: Iterable[Revocation], check: Iterable[Permission]): Boolean =
    Permission.impliesOne(given.map(_.perm), check)

  def reduce(rev: Iterable[Revocation]): Set[Revocation] = {
    val ps = Permission.reduce(rev.map(_.perm))
    ps.map(Revocation.apply)
  }

  def union(one: Set[Revocation], two: Set[Revocation]): Set[Revocation] =
    Permission.union(one.map(_.perm), two.map(_.perm)).map(apply)

  def diff(current: Set[Revocation], remove: Set[Revocation]): Set[Revocation] =
    Permission.diff(current.map(_.perm), remove.map(_.perm)).map(apply)
}

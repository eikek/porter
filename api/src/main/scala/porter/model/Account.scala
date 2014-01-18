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
case class Account(name: Ident,
  props: Properties = Map.empty,
  groups: Set[Ident] = Set.empty,
  secrets: Seq[Secret] = Seq.empty) extends Serializable {

  def updatedProps(f: Properties => Properties) =  copy(props = f(props))
  def updatedGroups(f: Set[Ident] => Set[Ident]) = copy(groups = f(groups))
  def updatedSecrets(f: Seq[Secret] => Seq[Secret]) = copy(secrets = f(secrets))

  /**
   * Replaces (or just adds) a secret with same name with the given one.
   *
   * @param s
   * @return
   */
  def changeSecret(s: Secret) = updatedSecrets { secs =>
    secs.filterNot(_.name == s.name) :+ s
  }

  def memberOf(group: Ident) = groups contains group
}

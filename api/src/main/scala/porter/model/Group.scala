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
case class Group(name: Ident,
  props: Properties = Map.empty,
  rules: Set[String] = Set.empty) extends Serializable {

  def updatedProps(f: Properties => Properties) =  copy(props = f(props))
  def updatedRules(f: Set[String] => Set[String]) = copy(rules = f(rules))
}

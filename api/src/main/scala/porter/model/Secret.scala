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

import scala.io.Codec

@SerialVersionUID(20131121)
case class Secret(name: Ident, data: Vector[Byte]) extends Serializable {
  import scala.io.Codec
  override def toString = s"Secret(${name.name}, ***)"
  lazy val asString = new String(data.toArray, Codec.UTF8.name)
}
object Secret {
  def apply(name: Ident, data: String): Secret = Secret(name, data.getBytes(Codec.UTF8.name).toVector)
  def apply(name: Ident, data: Array[Byte]): Secret = Secret(name, data.toVector)
}
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

package porter.app.openid.common

import porter.util.Hash

sealed trait SessionType {
  def name: String
  def hash(message: Vector[Byte]): Vector[Byte]
  override def toString = s"SessionType($name)"
}

object SessionType {

  object NoEncryption extends SessionType {
    val name = "no-encryption"
    def hash(message: Vector[Byte]) = sys.error("unsupported")
  }

  object DHSha1 extends SessionType {
    val name = "DH-SHA1"
    def hash(message: Vector[Byte]) = Hash.sha1(message)
  }

  object DHSha256 extends SessionType {
    val name = "DH-SHA256"
    def hash(message: Vector[Byte]) = Hash.sha256(message)
  }

  def apply(s: String) = s.toUpperCase match {
    case "DH-SHA1" => Some(DHSha1)
    case "DH-SHA256" => Some(DHSha256)
    case "NO-ENCRYPTION" => Some(NoEncryption)
    case _ => None
  }
}


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

sealed trait AssocType {
  def name: String
  def crypt: Crypt.Hmac
  override def toString = s"AssocType($name)"
}

object AssocType {

  object HmacSha1 extends AssocType {
    val name = "HMAC-SHA1"
    val crypt = Crypt.HmacSha1
  }

  object HmacSha256 extends AssocType {
    val name = "HMAC-SHA256"
    val crypt = Crypt.HmacSha256
  }

  def apply(s: String) = s.toUpperCase match {
    case "HMAC-SHA1" => Some(HmacSha1)
    case "HMAC-SHA256" => Some(HmacSha256)
    case _ => None
  }
}


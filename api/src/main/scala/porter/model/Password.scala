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

import porter.model.PasswordCrypt.Bcrypt

/**
 * Factory for password secrets. Usage:
 * {{{
 *   import porter.model.Password
 *   import porter.model.PasswordCrypt._
 *   Password("test")
 *   Password(Scrypt())("test")
 *   Password("password.1", Bcrypt(12))("test")
 * }}}
 * Verify password secrets:
 * {{{
 *   val secret = Password("testpw")
 *   Password.verify("testpw", secret) // returns `true`
 * }}}
 *
 */
object Password {
  private def secretName(num: Int) = Ident(s"password.$num")

  def create(name: Ident, crypt: PasswordCrypt)(plain: String): Secret = Secret(name, crypt(plain))

  def apply = create(secretName(0), Bcrypt())_
  def apply(name: Ident) = create(name, Bcrypt())_
  def apply(crypt: PasswordCrypt) = create(secretName(0), crypt)_
  def apply(name: Ident, crypt: PasswordCrypt) = create(name, crypt)_

  def crypted(cryptedPassword: String) = cryptedPassword match {
    case PasswordCrypt(v) => Secret(secretName(0), cryptedPassword)
    case _ => sys.error("The given password is not crypted.")
  }
  def verify(plain: String, secret: Secret): Boolean = PasswordCrypt.verify(plain, secret.asString)
}

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

package porter.auth

object PasswordValidator extends Validator {
  import porter.model._
  import Password._

  def authenticate(token: AuthToken) = {
    val userpass = token.credentials collect { case c: PasswordCredentials => c }
    val secrets = token.account.secrets.filter(_.name.name.startsWith("password."))
    userpass.foldLeft(token) { (token, up) =>
      secrets.foldLeft(token) { (token, sec) =>
        if (verifyPassword(up.password, sec.asString)) token.vote(sec.name -> Vote.Success)
        else token.vote(sec.name -> Vote.Failed())
      }
    }
  }

  @scala.annotation.tailrec
  private def verifyPassword(plain: String, hashed: String): Boolean =
    if (hashed startsWith "$shiro1$") verifyPassword(plain, hashed.replace("$shiro1$", "digest$"))
    else PasswordCrypt.verify(plain, hashed)

}

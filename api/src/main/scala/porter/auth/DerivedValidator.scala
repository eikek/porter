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

import porter.model.DerivedCredentials
import porter.util.Hash

object DerivedValidator extends Validator {

  def authenticate(token: AuthToken) = {
    val derived = token.credentials.collect { case dc: DerivedCredentials => dc }
    val found = for {
      cred <- derived
      accsecr <- token.account.secrets.find(s => s.name == cred.secret.name)
    } yield accsecr -> (!cred.isExpired && cred.secret.data == Hash.sha512(accsecr.data))
    found.foldLeft(token) { case (t, (s, bool)) =>
      if (bool) t vote (s.name -> Vote.Success)
      else t vote (s.name -> Vote.Failed())
    }
  }
}

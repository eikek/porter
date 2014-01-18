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

import porter.util.Hash

object DigestHa1 {

  /**
   * Creates a secret that holds the HA1 value used for authentication
   * with the http digest method.
   *
   * @param name the secret name
   * @param user the account
   * @param realm
   * @param plainPassword
   * @return
   */
  def create(name: Ident)(user: Ident, realm: String, plainPassword: String): Secret =
    Secret(name, Hash.md5String(user.name +":"+ realm +":"+ plainPassword))

  def apply(user: Ident, realm: String, plainPassword: String) = create("digestmd5.0")(user, realm, plainPassword)

}

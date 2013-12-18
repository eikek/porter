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

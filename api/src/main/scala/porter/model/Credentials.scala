package porter.model

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 19:20
 */
@SerialVersionUID(20131122)
trait Credentials extends Serializable

@SerialVersionUID(20131122)
trait AccountCredentials extends Credentials {
  def accountName: Ident
}

@SerialVersionUID(20131122)
case class PasswordCredentials(accountName: Ident, password: String) extends AccountCredentials {
  override def toString = s"${getClass.getSimpleName}(${accountName.name}, ***)"
}

@SerialVersionUID(20131130)
case class DigestCredentials(accountName: Ident,
                             method: String,
                             uri: String,
                             serverNonce: String,
                             response: String,
                             qop: Option[DigestQop] = None) extends AccountCredentials

@SerialVersionUID(20131130)
sealed trait DigestQop extends Serializable {
  def name: String
  def cnonce: String
  def nonceCount: String
}
@SerialVersionUID(20131130)
case class DigestAuthInt(cnonce: String, nonceCount: String, requestMd5: String) extends DigestQop {
  val name = "auth-int"
}
@SerialVersionUID(20131130)
case class DigestAuth(cnonce: String, nonceCount: String) extends DigestQop {
  val name = "auth"
}
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
class PasswordCredentials(val accountName: Ident, val password: String) extends AccountCredentials {
  override def toString = s"${getClass.getSimpleName}(${accountName.name}, ***)"
}
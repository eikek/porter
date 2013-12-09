package porter.auth

import porter.model._
import porter.model.Account
import porter.model.Realm

/**
 * @since 22.11.13 19:16
 */
@SerialVersionUID(20131122)
final case class AuthToken(realm: Realm,
                           account: Account,
                           credentials: Set[Credentials],
                           votes: Map[Ident, Vote] = Map.empty) extends Serializable {

  def vote(v: (Secret, Vote)) = copy(votes = votes + (v._1.name -> v._2))

  def toResult = AuthResult(realm, account.name, votes, account.props)
}

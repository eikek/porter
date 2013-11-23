package porter.auth

import porter.model._
import porter.model.Account
import porter.model.Realm

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 19:16
 */
@SerialVersionUID(20131122)
final case class AuthToken(realm: Realm,
                           account: Account,
                           credentials: Set[Credentials],
                           votes: Map[Ident, Vote] = Map.empty) extends Serializable {

  lazy val successCount = votes.values.foldLeft(0) { (s, v) =>
    s + (if (v == Vote.Success) 1 else 0)
  }

  lazy val failedCount = votes.size - successCount

  /**
   * Returns `true` if there is at least one successful vote and
   * no failed ones.
   */
  lazy val oneSuccess = successCount > 0 && failedCount == 0

  def vote(v: (Secret, Vote)) = copy(votes = votes + (v._1.name -> v._2))
}

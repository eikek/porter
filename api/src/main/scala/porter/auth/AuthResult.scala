package porter.auth

import porter.model.{Realm, Ident}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 17:33
 */
@SerialVersionUID(20131123)
final case class AuthResult(realm: Realm, accountId: Ident, votes: Map[Ident, Vote]) extends Serializable {

  lazy val successCount = votes.values.foldLeft(0) { (s, v) =>
    s + (if (v == Vote.Success) 1 else 0)
  }

  lazy val failedCount = votes.size - successCount

  /**
   * Returns `true` if there is at least one successful vote and
   * no failed ones.
   */
  lazy val oneSuccess = successCount > 0 && failedCount == 0

}

package porter.auth

import porter.model.{Properties, Realm, Ident}

@SerialVersionUID(20131123)
final case class AuthResult(realm: Realm, accountId: Ident, votes: Map[Ident, Vote], props: Properties) extends Serializable {

  lazy val successCount = votes.values.foldLeft(0) { (s, v) =>
    s + (if (v == Vote.Success) 1 else 0)
  }

  lazy val failedCount = votes.size - successCount

  lazy val oneSuccess = OneSuccessfulVote(this)

}

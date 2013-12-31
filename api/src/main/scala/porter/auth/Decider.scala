package porter.auth

/**
 * Decides for a given [[porter.auth.AuthResult]] if it represents
 * a successful authentication.
 *
 */
trait Decider extends (AuthResult => Boolean)

/**
 * Returns `true` if there is at least one successful vote and
 * no failed ones. It also checks the properties
 * [[porter.model.Property.accountDisabled]] and [[porter.model.Property.accountLocked]].
 */
object OneSuccessfulVote extends Decider {
  import Decider._
  def apply(result: AuthResult) =
    notDisabled(result) && result.successCount > 0 && result.failedCount == 0
}

/**
 * Returns `true` if there is at least one successful vote while
 * it does not look at any failed ones. It also checks the properties
 * [[porter.model.Property.accountDisabled]] and [[porter.model.Property.accountLocked]].
 */
object SomeSuccessfulVote extends Decider {
  import Decider._
  def apply(result: AuthResult) = notDisabled(result) && result.successCount > 0
}

object Decider {
  import porter.model.Property._

  def isDisabled(result: AuthResult) =
    accountDisabled.isTrue(result.props) || accountLocked.isTrue(result.props)

  def notDisabled(result: AuthResult) = !isDisabled(result)

}

package porter.auth

/**
 * Decides for a given [[porter.auth.AuthResult]] if it represents
 * a successful authentication.
 *
 */
trait Decider extends (AuthResult => Boolean)

/**
 * Returns `true` if there is at least one successful vote and
 * no failed ones. It also checks a property of name "account.disabled";
 * if this exists and is set to "true", the decider returns false.
 */
object OneSuccessfulVote extends Decider {
  import Decider._
  def apply(result: AuthResult) =
    notDisabled(result) && result.successCount > 0 && result.failedCount == 0
}

/**
 * Returns `true` if there is at least one successful vote while
 * it does not look at any failed ones. It also checks a property
 * of name "account.disabled"; if this exists and is set to "true",
 * the decider returns false.
 */
object SomeSuccessfulVote extends Decider {
  import Decider._
  def apply(result: AuthResult) = notDisabled(result) && result.successCount > 0
}

object Decider {

  def isDisabled(result: AuthResult) =
    result.props.get("account.disabled").exists(_.toLowerCase == "true")

  def notDisabled(result: AuthResult) = !isDisabled(result)

}

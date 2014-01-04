package porter.app.akka.api

import akka.actor.ActorRef
import porter.model.{Account, Credentials, Ident}
import porter.auth.{AuthResult, OneSuccessfulVote, Decider}
import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import porter.app.akka.Porter.Messages.authc._
import porter.app.akka.Porter.Messages.authz._
import porter.app.akka.Porter.Messages.store._
import porter.app.akka.api.PorterMain.UpdateAuthProps

object PorterUtil {
  /**
   * A future that authenticates at the given porter actor. If the future completes
   * successfully, the user is successfully authenticated. Otherwise the authentication
   * failed.
   *
   * @param porter porter actor doing the authentication
   * @param realm the realm to look for accounts
   * @param creds given credentials
   * @param decider a function to decide whether the authentication is successful, given the
   *                response from the porter actor
   * @param ec
   * @param timeout
   * @return
   */
  def authenticationFuture(porter: ActorRef, realm: Ident, creds: Set[Credentials], decider: Decider)
                          (implicit ec: ExecutionContext, timeout: Timeout): Future[AuthResult] = {
    import akka.pattern.ask
    val f = (porter ? Authenticate(realm, creds)).mapTo[AuthenticateResp].map {
      case AuthenticateResp(Some(r), _) if decider(r) =>
        porter ! UpdateAuthProps(realm, creds, success = true)
        r
      case _ =>
        porter ! UpdateAuthProps(realm, creds, success = false)
        sys.error("Invalid Credentials")
    }
    f
  }

  /**
   * A future that loads an [[porter.model.Account]] using the given realm and account identifier.
   *
   * @param porter
   * @param realm
   * @param account
   * @param ec
   * @param timeout
   * @return
   */
  def accountFuture(porter: ActorRef, realm: Ident, account: Ident)
                   (implicit ec: ExecutionContext, timeout: Timeout) = {
    import akka.pattern.ask
    (porter ? FindAccounts(realm, Set(account))).mapTo[FindAccountsResp].map {
      case FindAccountsResp(a, _) => a.headOption
      case _ => None
    }
  }

  /**
   * A future that authenticates at the given porter actor and retrieves the
   * [[porter.model.Account]] to the authenticated user if authentication was
   * successful. If authentication failed, the future fails.
   *
   * @param porter
   * @param realm
   * @param creds
   * @param decider
   * @param ec
   * @param timeout
   * @return
   */
  def authenticateAccount(porter: ActorRef,
                          realm: Ident,
                          creds: Set[Credentials],
                          decider: Decider = OneSuccessfulVote)
                         (implicit ec: ExecutionContext, timeout: Timeout): Future[(AuthResult, Account)] = {
    val f = for {
      auth <- authenticationFuture(porter, realm, creds, decider)
      acc <- accountFuture(porter, realm, auth.accountId)
    } yield (auth, acc)
    f.flatMap {
      case (result, Some(a)) => Future.successful((result, a))
      case _ => Future.failed(new RuntimeException("Invalid credentials"))
    }
  }

  /**
   * A future that checks the given permissions against the policy of the
   * given account.
   *
   * @param porter
   * @param realm
   * @param account
   * @param perms
   * @param ec
   * @param timeout
   * @return
   */
  def authorize(porter: ActorRef, realm: Ident, account: Ident, perms: Set[String])
               (implicit ec: ExecutionContext, timeout: Timeout): Future[Boolean] = {
    import akka.pattern.ask
    val f = (porter ? Authorize(realm, account, perms)).mapTo[AuthorizeResp]
    f.map(_.authorized)
  }
}

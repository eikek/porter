package porter.app.akka

import akka.actor.ActorRef
import porter.model._
import porter.auth.{AuthResult, OneSuccessfulVote, Decider}
import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import porter.app.akka.Porter.Messages.authc._
import porter.app.akka.Porter.Messages.authz._
import porter.app.akka.Porter.Messages.store._
import porter.app.akka.Porter.Messages.mutableStore._
import porter.app.akka.api.PorterMain.UpdateAuthProps

/**
 * Utility functions combining different porter messages.
 *
 */
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

  /**
   * Future set loads the account with the given name and applies the function
   * to it and updates the store with the result.
   *
   * @param porterRef
   * @param realm
   * @param account
   * @param alter
   * @param ec
   * @param timeout
   * @return
   */
  def updateAccount(porterRef: ActorRef, realm: Ident, account: Ident, alter: Account => Account)
                   (implicit ec: ExecutionContext, timeout: Timeout): Future[OperationFinished] = {
    import akka.pattern.ask
    import porter.util._
    for {
      resp <- (porterRef ? FindAccounts(realm, Set(account))).mapTo[FindAccountsResp]
      first <- Future.immediate(resp.accounts.headOption, s"Account '${account.name}' not found")
      op <- (porterRef ? UpdateAccount(realm, alter(first))).mapTo[OperationFinished]
    } yield op
  }

  /**
   * Future that first loads an account using the given one and performs an update
   * only if an empty response is returned.
   *
   * @param porterRef
   * @param realm
   * @param account
   * @param ec
   * @param timeout
   * @return
   */
  def createNewAccount(porterRef: ActorRef, realm: Ident, account: Account)
                      (implicit ec: ExecutionContext, timeout: Timeout): Future[Account] = {
    import akka.pattern.ask
    import porter.util._
    for {
      resp <- (porterRef ? FindAccounts(realm, Set(account.name))).mapTo[FindAccountsResp]
      empty <- Future.immediate(if (resp.accounts.isEmpty) resp else sys.error(s"Account '${account.name}' already exists"))
      upd <- (porterRef ? UpdateAccount(realm, account)).mapTo[OperationFinished]
    } yield if (upd.result) account else sys.error("Unable to create account")
  }

  /**
   * Future that loads a group with the given name and stores the result of the
   * given `alter` function.
   *
   * @param porterRef
   * @param realm
   * @param group
   * @param alter
   * @param ec
   * @param timeout
   * @return
   */
  def updateGroup(porterRef: ActorRef, realm: Ident, group: Ident, alter: Group => Group)
                 (implicit ec: ExecutionContext, timeout: Timeout): Future[OperationFinished] = {
    import akka.pattern.ask
    import porter.util._
    for {
      resp <- (porterRef ? FindGroups(realm, Set(group))).mapTo[FindGroupsResp]
      first <- Future.immediate(resp.groups.headOption, s"Group '${group.name}' not found")
      op <- (porterRef ? UpdateGroup(realm, alter(first))).mapTo[OperationFinished]
    } yield op
  }

  /**
   * Loads a realm with the given name and stores the result of the given
   * `alter` function.
   *
   * @param porterRef
   * @param realm
   * @param alter
   * @param ec
   * @param timeout
   * @return
   */
  def updateRealm(porterRef: ActorRef, realm: Ident, alter: Realm => Realm)
                 (implicit ec: ExecutionContext, timeout: Timeout): Future[OperationFinished] = {
    import akka.pattern.ask
    import porter.util._
    for {
      resp <- (porterRef ? FindRealms(Set(realm))).mapTo[FindRealmsResp]
      first <- Future.immediate(resp.realms.headOption, s"Realm '${realm.name}' not found.")
      op <- (porterRef ? UpdateRealm(alter(first))).mapTo[OperationFinished]
    } yield op
  }
}

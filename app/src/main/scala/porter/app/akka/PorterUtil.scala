/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porter.app.akka

import akka.actor.{ActorSelection, ActorRef}
import porter.model._
import porter.auth.{AuthResult, OneSuccessfulVote, Decider}
import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import porter.client.messages._

/**
 * Utility functions combining different porter messages.
 *
 */
object PorterUtil {

  /**
   * A future that authenticates at the given porter actor. It updates login properties
   * of the account upon completion.
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
  def authenticationFuture(porter: PorterRef, realm: Ident, creds: Set[Credentials], decider: Decider)
                          (implicit ec: ExecutionContext, timeout: Timeout): Future[AuthenticateResp] = {
    val f = (porter ? Authenticate(realm, creds)).mapTo[AuthenticateResp].map {
      case r @ AuthenticateResp(Some(res)) if decider(res) =>
        porter ! UpdateAuthProps(realm, creds, success = true)
        r
      case r @ _ =>
        porter ! UpdateAuthProps(realm, creds, success = false)
        AuthenticateResp(None)
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
  def findAccount(porter: PorterRef, realm: Ident, account: Ident)
                   (implicit ec: ExecutionContext, timeout: Timeout): Future[Option[Account]] = {
    import akka.pattern.ask
    (porter ? FindAccounts(realm, Set(account))).mapTo[FindAccountsResp].map {
      case FindAccountsResp(a) => a.headOption
    }
  }

  /**
   * Finds a single group with the given identifier.
   *
   * @param porterRef
   * @param realm
   * @param group
   * @param ec
   * @param timeout
   * @return
   */
  def findGroup(porterRef: PorterRef, realm: Ident, group: Ident)
               (implicit ec: ExecutionContext, timeout: Timeout): Future[Option[Group]] = {
    import akka.pattern.ask
    (porterRef ? FindGroups(realm, Set(group))).mapTo[FindGroupsResp].map {
      case FindGroupsResp(g) => g.headOption
    }
  }

  /**
   * Finds a single realm given its id.
   *
   * @param porterRef
   * @param realm
   * @param ec
   * @param timeout
   * @return
   */
  def findRealm(porterRef: PorterRef, realm: Ident)
               (implicit ec: ExecutionContext, timeout: Timeout): Future[Option[Realm]] = {{
    import akka.pattern.ask
    (porterRef ? FindRealms(Set(realm))).mapTo[FindRealmsResp].map {
      case FindRealmsResp(r) => r.headOption
    }
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
  def authenticateAccount(porter: PorterRef,
                          realm: Ident,
                          creds: Set[Credentials],
                          decider: Decider = OneSuccessfulVote)
                         (implicit ec: ExecutionContext, timeout: Timeout): Future[(AuthResult, Account)] = {
    val f = for {
      auth <- authenticationFuture(porter, realm, creds, decider)
      if auth.result.isDefined
      acc <- findAccount(porter, realm, auth.result.get.accountId)
    } yield (auth.result.get, acc)
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
  def authorize(porter: PorterRef, realm: Ident, account: Ident, perms: Set[String])
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
  def updateAccount(porterRef: PorterRef, realm: Ident, account: Ident, alter: Account => Account)
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
  def createNewAccount(porterRef: PorterRef, realm: Ident, account: Account)
                      (implicit ec: ExecutionContext, timeout: Timeout): Future[Account] = {
    import akka.pattern.ask
    import porter.util._
    for {
      resp <- (porterRef ? FindAccounts(realm, Set(account.name))).mapTo[FindAccountsResp]
      empty <- Future.immediate(if (resp.accounts.isEmpty) resp else sys.error(s"Account '${account.name}' already exists"))
      upd <- (porterRef ? UpdateAccount(realm, account)).mapTo[OperationFinished]
    } yield
      if (upd.success) account
      else upd.exception match {
        case Some(error) => throw error
        case _ => sys.error("Unable to create account")
      }
  }

  /**
   * Future that authenticates to get the account and then updates the password with
   * the given plain password.
   *
   * @param porterRef
   * @param realm
   * @param current
   * @param newSecrets
   * @param decider
   * @param ec
   * @param timeout
   * @return
   */
  def changePassword(porterRef: PorterRef, realm: Ident, current: Set[Credentials], newSecrets: Seq[Secret], decider: Decider = OneSuccessfulVote)
                    (implicit ec: ExecutionContext, timeout: Timeout): Future[Account] = {

    import akka.pattern.ask
    for {
      account <- authenticateAccount(porterRef, realm, current, decider).map(_._2)
      nacc <- Future.successful(newSecrets.foldLeft(account) { (acc, s) => acc.changeSecret(s) })
      upd <- (porterRef ? UpdateAccount(realm, nacc)).mapTo[OperationFinished]
    } yield
      if (upd.success) nacc
      else upd.exception match {
        case Some(error) => throw error
        case _ => sys.error("Unable to change password")
      }
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
  def updateGroup(porterRef: PorterRef, realm: Ident, group: Ident, alter: Group => Group)
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
  def updateRealm(porterRef: PorterRef, realm: Ident, alter: Realm => Realm)
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

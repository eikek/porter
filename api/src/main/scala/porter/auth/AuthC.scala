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

package porter.auth

import porter.store.StoreProvider
import scala.concurrent.{ExecutionContext, Future}

trait AuthC {
  self: StoreProvider with ValidatorProvider =>

  import porter.model._

  private val auth = AuthC.authenticate(_: AuthToken, validators)

  final def authenticate(realm: Ident, creds: Set[Credentials], decider: Decider)(implicit ec: ExecutionContext): Future[Boolean] = {
    import PropertyList._
    authenticate(realm, creds).map { result =>
      val decision = decider(result)
      val props =
        if (decision) lastLoginTime.current.andThen(successfulLogins.increment)
        else failedLogins.increment
      mutableStore(realm) match {
        case Some(ms) =>
          for {
            acc <- store.findAccountsFor(realm, creds)
            if acc.nonEmpty
            upd <- ms.updateAccount(realm, acc.head.updatedProps(props))
          } yield upd
        case _ =>
      }
      decision
    }
  }

  final def authenticate(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext): Future[AuthResult] = {
    for {
      r <- store.findRealms(Set(realm))
      if r.nonEmpty
      a <- store.findAccountsFor(realm, creds).map(findAccount(creds, _).getOrElse(sys.error("Account not found.")))
    } yield auth(AuthToken(r.toList(0), a, creds)).toResult
  }

  private def findAccount(creds: Set[Credentials], accounts: Iterable[Account]): Option[Account] =
    accounts.headOption.orElse {
      val name = creds.collect({ case pw: AccountCredentials => pw.accountName.name})
      name.headOption.map(Account.apply(_))
    }

  final def authenticate(realm: Ident, account: Account, creds: Set[Credentials])(implicit ec: ExecutionContext): Future[AuthResult] = {
    for {
      r <- store.findRealms(Set(realm))
      if r.nonEmpty
    } yield auth(AuthToken(r.toList(0), account, creds)).toResult
  }

}

object AuthC {

  def authenticate(token: AuthToken, authenticators: Iterable[Validator]): AuthToken =
    (token /: authenticators) { (token, auther) => auther authenticate token }

}

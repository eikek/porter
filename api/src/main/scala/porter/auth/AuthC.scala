package porter.auth

import porter.store.StoreProvider
import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 23:26
 */
trait AuthC {
  self: StoreProvider with AuthenticatorProvider =>

  import porter.model._

  def authenticate(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext): Future[AuthResult] = {
    for {
      r <- store.findRealms(Set(realm))
      if r.nonEmpty
      a <- store.findAccountsFor(realm, creds)
      if a.nonEmpty
    } yield auth(AuthToken(r.toList(0), a.take(1).toList(0), creds)).toResult
  }

  def authenticate(realm: Ident, account: Account, creds: Set[Credentials])(implicit ec: ExecutionContext): Future[AuthResult] = {
    for {
      r <- store.findRealms(Set(realm))
      if r.nonEmpty
    } yield auth(AuthToken(r.toList(0), account, creds)).toResult
  }

  private def auth(token: AuthToken): AuthToken =
    (token /: authenticators) { (token, auther) => auther authenticate token }
}

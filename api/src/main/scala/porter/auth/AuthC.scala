package porter.auth

import porter.store.StoreProvider
import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 22.11.13 23:26
 */
trait AuthC {
  self: StoreProvider with ValidatorProvider =>

  import porter.model._

  private val auth = AuthC.authenticate(_: AuthToken, validators)

  def authenticate(realm: Ident, creds: Set[Credentials], decider: Decider)(implicit ec: ExecutionContext): Future[Boolean] = {
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

}

object AuthC {

  def authenticate(token: AuthToken, authenticators: Iterable[Validator]): AuthToken =
    (token /: authenticators) { (token, auther) => auther authenticate token }

}

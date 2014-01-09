package porter.app.akka.api

import akka.actor.{ActorLogging, Props, Actor, ActorRef}
import akka.util.Timeout
import porter.app.akka.api.StoreActor.messages._
import porter.client.Messages.mutableStore._
import porter.client.Messages.auth.{RetrieveServerNonceResp, RetrieveServerNonce}
import porter.auth.Nonce

/**
 * Some additional functions.
 *
 */
class ExtrasActor(store: ActorRef, mstore: ActorRef, ruleFactory: ActorRef, policy: ActorRef) extends Actor with ActorLogging {
  implicit val timeout = Timeout(5000)
  import context.dispatcher
  import akka.pattern.ask
  import akka.pattern.pipe
  import scala.concurrent.duration._

  def receive = {
    case UpdateAuthProps(realm, creds, success) =>
      import porter.model.PropertyList._
      lazy val props =
        if (success) lastLoginTime.current.andThen(successfulLogins.increment)
        else failedLogins.increment
      val f = for {
        acc <- (store ? FindAccountsFor(realm, creds)).mapTo[FindAccountsResp]
        if acc.accounts.nonEmpty
        upd <- (mstore ? UpdateAccount(realm, acc.accounts.head.updatedProps(props))).mapTo[OperationFinished]
      } yield upd
      f.onFailure { case x =>
        log.error(x, "Cannot update acount properties")
        OperationFinished(result = false)
      }
      f pipeTo sender

    case RetrieveServerNonce(valid) =>
      sender ! RetrieveServerNonceResp(Nonce.generateNonce(valid.getOrElse(2.minutes)))
  }
}

object ExtrasActor {
  def apply(store: ActorRef, mstore: ActorRef, ruleFactory: ActorRef, policy: ActorRef): Props =
    Props(classOf[ExtrasActor], store, mstore, ruleFactory, policy)
}
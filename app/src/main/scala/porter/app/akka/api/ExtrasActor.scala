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

package porter.app.akka.api

import akka.actor.{ActorLogging, Props, Actor, ActorRef}
import akka.util.Timeout
import porter.client.messages._
import porter.auth.Nonce
import porter.app.akka.api.StoreActor.FindAccountsFor

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
      f.recover { case x =>
        OperationFinished.failure(x)
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
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

import porter.store.MutableStore
import porter.model.Ident
import akka.actor._
import akka.util.Timeout
import scala.Some
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future

class MutableStoreActor(stores: List[(Set[Ident], MutableStore)]) extends Actor with ActorLogging {
  import MutableStoreActor._
  import messages._

  private var workerCreated = 0
  private var workerActive = 0
  
  private def findStore(realm: Ident) =
    for {
      (id, mstore) <- stores.find(_._1 contains realm) orElse stores.headOption
    } yield mstore

  private def withStore(realm: Ident, f: ActorRef => Unit, g: => Unit) {
    findStore(realm) match {
      case Some(s) =>
        val a = context.watch(context.actorOf(workerProps(s), name = s"mstore$workerCreated"))
        workerCreated += 1; workerActive += 1
        f(a)
        a ! PoisonPill
      case None => g
    }
  }

  def receive = {
    case pm: MutableStoreMessage =>
      withStore(pm.realmId, _ forward pm, sender ! failed)

    case Terminated(ref) =>
      workerActive -= 1
      log.debug(s"Actor $ref terminated. Active mstore workers: $workerActive")
  }
}

object MutableStoreActor {
  import porter.model._

  val messages = porter.client.Messages.mutableStore
  import messages._

  def apply(stores: List[(Set[Ident], MutableStore)]) = Props(classOf[MutableStoreActor], stores)

  private def workerProps(store: MutableStore) = Props(classOf[WorkerActor], store)

  private def failed = OperationFinished(result = false)
  private def finish(result: Boolean) = OperationFinished(result)

  private class WorkerActor(store: MutableStore) extends Actor with ActorLogging {
    import akka.pattern.pipe
    import context.dispatcher
    implicit val timeout = Timeout(3000)

    private def fail: PartialFunction[Throwable, OperationFinished] = {
      case x =>
        log.error(x, "Mutable store operation failed")
        failed
    }

    private def exec(result: Try[Future[Boolean]]) {
      result match {
        case Success(f) => f.map(finish).recover(fail) pipeTo sender
        case Failure(ex) => sender ! fail(ex)
      }
    }

    def receive = {
      case UpdateRealm(realm) =>
        exec(Try(store.updateRealm(realm)))
      case DeleteRealm(realm) =>
        exec(Try(store.deleteRealm(realm)))
      case UpdateAccount(realm, account) =>
        exec(Try(store.updateAccount(realm, account)))
      case DeleteAccount(realm, account) =>
        exec(Try(store.deleteAccount(realm, account)))
      case UpdateGroup(realm, group) =>
        exec(Try(store.updateGroup(realm, group)))
      case DeleteGroup(realm, group) =>
        exec(Try(store.deleteGroup(realm, group)))
    }

    override def preRestart(reason: Throwable, message: Option[Any]) = {
      super.preRestart(reason, message)
      store.close()
    }

    override def postStop() = {
      super.postStop()
      store.close()
    }
  }
}
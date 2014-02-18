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

import scala.concurrent.Future
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import akka.actor._
import akka.util.Timeout
import porter.store.Store
import porter.client.Messages.store._

class StoreActor(stores: List[Store]) extends Actor with ActorLogging {
  import StoreActor._

  val readonly = stores.zipWithIndex.map { case (s, i) =>
    context.actorOf(readOnlyProps(s), name = s"store$i")
  }
  if (readonly.isEmpty) context.become(empty)

  var workerCreated = 0
  var workerActive = 0

  def receive = normal

  def empty: Receive = {
    case sm: StoreMessage =>
      sender ! Status.Failure(new Exception("No stores provided."))
  }

  def normal: Receive = {
    case req: FindRealms => receiveRealms(sender, req)
    case req: FindAccounts => receiveAccounts(sender, req)
    case req: FindAccountsFor => receiveAccounts(sender, req)
    case req: FindGroups => receiveGroups(sender, req)
    case req: GetAllGroups => receiveGroups(sender, req)
    case req: GetAllAccounts => receiveAccounts(sender, req)
    case GetAllRealms => receiveRealms(sender, GetAllRealms)
    case Terminated(ref) =>
      workerActive -= 1
      log.debug(s"Actor $ref termintated. Active workers: $workerActive")
  }

  private def receiveRealms(client: ActorRef, req: PorterMessage) {
    val name = s"realms$workerCreated"
    workerCreated += 1; workerActive += 1
    context.watch(context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
      type Res = FindRealmsResp
      def empty = FindRealmsResp(Set())
      def merge(r1: Res, r2: Res) = FindRealmsResp(r1.realms ++ r2.realms)
      object Extr {
        def unapply(a: Any) = a match {
          case r: FindRealmsResp => Some(r)
          case _ => None
        }
      }
    }), name))
  }

  private def receiveAccounts(client: ActorRef, req: PorterMessage) {
    val name = s"accounts$workerCreated"
    workerCreated += 1; workerActive += 1
    context.watch(context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
      type Res = FindAccountsResp
      object Extr {
        def unapply(a: Any) = a match {
          case r: FindAccountsResp => Some(r)
          case _ => None
        }
      }
      val empty = FindAccountsResp(Set())

      def merge(r1: FindAccountsResp, r2: FindAccountsResp) =
        FindAccountsResp(r1.accounts ++ r2.accounts)
    }), name))
  }

  private def receiveGroups(client: ActorRef, req: PorterMessage) {
    val name = s"groups$workerCreated"
    workerCreated += 1; workerActive += 1
    context.watch(context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
      type Res = FindGroupsResp
      def merge(r1: this.type#Res, r2: this.type#Res) =
        FindGroupsResp(r1.groups++r2.groups)
      val empty = FindGroupsResp(Set())

      object Extr {
        def unapply(a: Any) = a match {
          case r: FindGroupsResp => Some(r)
          case _ => None
        }
      }
      val request = req
    }), name))
  }

}

object StoreActor {
  import porter.model._

  def apply(stores: List[Store]) = Props(classOf[StoreActor], stores)

  private def readOnlyProps(store: Store) = Props(classOf[ReadonlyStore], store)

  private class ReadonlyStore(store: Store) extends Actor {
    import akka.pattern.pipe
    import context.dispatcher
    implicit val timeout = Timeout(3000)

    private def exec(result: Try[Future[_]]) {
      result match {
        case Success(f) => f.recover({case ex => Status.Failure(ex) }) pipeTo sender
        case Failure(ex) => sender ! Status.Failure(ex)
      }
    }

    def receive = {
      case FindRealms(names) =>
        exec(Try(store.findRealms(names).map(r => FindRealmsResp(r.toSet))))
      case FindAccounts(realm, names) =>
        exec(Try(store.findAccounts(realm, names).map(s => FindAccountsResp(s.toSet))))
      case FindAccountsFor(realm, creds) =>
        exec(Try(store.findAccountsFor(realm, creds).map(s => FindAccountsResp(s.toSet))))
      case FindGroups(realm, names) =>
        exec(Try(store.findGroups(realm, names).map(s => FindGroupsResp(s.toSet))))
      case GetAllRealms =>
        exec(Try(store.allRealms.map(r => FindRealmsResp(r.toSet))))
      case GetAllAccounts(realm) =>
        exec(Try(store.allAccounts(realm).map(a => FindAccountsResp(a.toSet))))
      case GetAllGroups(realm) =>
        exec(Try(store.allGroups(realm).map(g => FindGroupsResp(g.toSet))))
    }

    override def preRestart(reason: Throwable, message: Option[Any]) = {
      super.preRestart(reason, message)
//      store.close()
    }

    override def postStop() = {
      super.postStop()
//      store.close()
    }
  }

  /** Returns all realms in a `FindRealmsResp` object */
  case object GetAllRealms extends StoreMessage

  case class FindAccountsFor(realm: Ident, creds: Set[Credentials]) extends StoreMessage

}
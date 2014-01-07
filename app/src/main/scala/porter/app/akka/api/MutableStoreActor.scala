package porter.app.akka.api

import porter.store.MutableStore
import porter.model.Ident
import akka.actor._
import akka.util.Timeout
import scala.Some
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future

/**
 * @since 05.12.13 23:00
 */
class MutableStoreActor(stores: List[(Set[Ident], MutableStore)]) extends Actor {
  import MutableStoreActor._
  import messages._

  private def findStore(realm: Ident) =
    stores.find({ case (id, a) => id.contains(realm) }).map(_._2)
      .orElse(stores.headOption.map(_._2))

  private def withStore(realm: Ident, f: ActorRef => Unit, g: => Unit) {
    findStore(realm) match {
      case Some(s) =>
        val a = context.actorOf(workerProps(s))
        f(a)
        a ! PoisonPill
      case None => g
    }
  }

  def receive = {
    case pm: MutableStoreMessage =>
      withStore(pm.realmId, _ forward pm, sender ! failed)
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
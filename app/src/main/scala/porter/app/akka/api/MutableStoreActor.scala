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
    case pm: RealmMessage =>
      withStore(pm.realmId, _ forward pm, sender ! failed(pm.id))
  }
}

object MutableStoreActor {
  import porter.model._
  import messages._

  def apply(stores: List[(Set[Ident], MutableStore)]) = Props(classOf[MutableStoreActor], stores)

  private def workerProps(store: MutableStore) = Props(classOf[WorkerActor], store)

  private def failed(id: Int) = OperationFinished(result = false, id)
  private def finish(id: Int)(result: Boolean) = OperationFinished(result, id)

  private class WorkerActor(store: MutableStore) extends Actor with ActorLogging {
    import akka.pattern.pipe
    import context.dispatcher
    implicit val timeout = Timeout(3000)

    private def fail(id: Int): PartialFunction[Throwable, OperationFinished] = {
      case x =>
        log.error(x, "Mutable store operation failed")
        failed(id)
    }

    private def exec(result: Try[Future[Boolean]], id: Int) {
      result match {
        case Success(f) => f.map(finish(id)).recover(fail(id)) pipeTo sender
        case Failure(ex) => sender ! fail(id)(ex)
      }
    }

    def receive = {
      case UpdateRealm(realm, id) =>
        exec(Try(store.updateRealm(realm)), id)
      case DeleteRealm(realm, id) =>
        exec(Try(store.deleteRealm(realm)), id)
      case UpdateAccount(realm, account, id) =>
        exec(Try(store.updateAccount(realm, account)), id)
      case DeleteAccount(realm, account, id) =>
        exec(Try(store.deleteAccount(realm, account)), id)
      case UpdateGroup(realm, group, id) =>
        exec(Try(store.updateGroup(realm, group)), id)
      case DeleteGroup(realm, group, id) =>
        exec(Try(store.deleteGroup(realm, group)), id)
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

  object messages {
    trait MutableStoreMessage extends PorterMessage
    case class UpdateRealm(realm: Realm, id: Int = 0) extends RealmMessage with MutableStoreMessage {
      val realmId = realm.id
    }
    case class DeleteRealm(realmId: Ident, id: Int = 0) extends RealmMessage with MutableStoreMessage
    case class UpdateAccount(realmId: Ident, account: Account, id: Int = 0) extends RealmMessage with MutableStoreMessage
    case class DeleteAccount(realmId: Ident, account: Ident, id: Int = 0) extends RealmMessage with MutableStoreMessage
    case class UpdateGroup(realmId: Ident, group: Group, id: Int = 0) extends RealmMessage with MutableStoreMessage
    case class DeleteGroup(realmId: Ident, group: Ident, id: Int = 0) extends RealmMessage with MutableStoreMessage
    case class OperationFinished(result: Boolean, id: Int) extends PorterMessage with MutableStoreMessage
  }
}
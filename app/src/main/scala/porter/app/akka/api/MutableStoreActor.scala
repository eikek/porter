package porter.app.akka.api

import porter.store.MutableStore
import porter.model.Ident
import akka.actor.{ActorRef, Props, Actor}
import akka.util.Timeout

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 05.12.13 23:00
 */
class MutableStoreActor(stores: List[(Set[Ident], MutableStore)]) extends Actor {
  import MutableStoreActor._

  val mutableStores =
    for ((list, s) <- stores)
    yield list -> context.actorOf(workerProps(s))

  private def findStore(realm: Ident) =
    mutableStores.find({ case (id, a) => id.contains(realm) }).map(_._2)
      .orElse(mutableStores.headOption.map(_._2))

  private def withStore(realm: Ident, f: ActorRef => Unit, g: => Unit) {
    findStore(realm) match {
      case Some(s) => f(s)
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

  private class WorkerActor(store: MutableStore) extends Actor {
    import akka.pattern.pipe
    import context.dispatcher
    implicit val timeout = Timeout(3000)

    def receive = {
      case UpdateRealm(realm, id) =>
        store.updateRealm(realm).map(finish(id)) pipeTo sender
      case DeleteRealm(realm, id) =>
        store.deleteRealm(realm).map(finish(id)) pipeTo sender
      case UpdateAccount(realm, account, id) =>
        store.updateAccount(realm, account).map(finish(id)) pipeTo sender
      case DeleteAccount(realm, account, id) =>
        store.deleteAccount(realm, account).map(finish(id)) pipeTo sender
      case UpdateGroup(realm, group, id) =>
        store.updateGroup(realm, group).map(finish(id)) pipeTo sender
      case DeleteGroup(realm, group, id) =>
        store.deleteGroup(realm, group).map(finish(id)) pipeTo sender
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
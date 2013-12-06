package porter.app.akka.api

import akka.actor.{ActorRef, Props, Actor}
import porter.store.Store
import akka.util.Timeout

/**
 * @since 05.12.13 15:24
 */
class StoreActor(stores: List[Store]) extends Actor {
  import StoreActor._

  val readonly = stores.map(s => context.actorOf(readOnlyProps(s)))

  def receive = {
    case req: FindRealms => receiveRealms(sender, req)
    case req: FindAccounts => receiveAccounts(sender, req)
    case req: FindAccountsFor => receiveAccounts(sender, req)
    case req: FindGroups => receiveGroups(sender, req)
    case req: GetAllGroups => receiveGroups(sender, req)
    case req: GetAllAccounts => receiveAccounts(sender, req)
    case req: GetAllRealms => receiveRealms(sender, req)
  }

  private def receiveRealms(client: ActorRef, req: PorterMessage) {
    context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
      type Res = FindRealmsResponse
      def empty = FindRealmsResponse(Set(), req.id)
      def merge(r1: Res, r2: Res) = FindRealmsResponse(r1.realms ++ r2.realms, r1.id)
      object Extr {
        def unapply(a: Any) = a match {
          case r: FindRealmsResponse => Some(r)
          case _ => None
        }
      }
    }))
  }

  private def receiveAccounts(client: ActorRef, req: PorterMessage) {
    context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
      type Res = FindAccountsResp
      object Extr {
        def unapply(a: Any) = a match {
          case r: FindAccountsResp => Some(r)
          case _ => None
        }
      }
      val empty = FindAccountsResp(Set(), req.id)

      def merge(r1: FindAccountsResp, r2: FindAccountsResp) =
        FindAccountsResp(r1.accounts ++ r2.accounts, r1.id)
    }))
  }

  private def receiveGroups(client: ActorRef, req: PorterMessage) {
    context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
      type Res = FindGroupsResp
      def merge(r1: this.type#Res, r2: this.type#Res) =
        FindGroupsResp(r1.groups++r2.groups, r1.id)
      val empty = FindGroupsResp(Set(), req.id)

      object Extr {
        def unapply(a: Any) = a match {
          case r: FindGroupsResp => Some(r)
          case _ => None
        }
      }
      val request = req
    }))
  }

}

object StoreActor {
  import porter.model._

  def props(stores: List[Store]) = Props(classOf[StoreActor], stores)

  private def readOnlyProps(store: Store) = Props(classOf[ReadonlyStore], store)

  private class ReadonlyStore(store: Store) extends Actor {
    import akka.pattern.pipe
    import context.dispatcher
    implicit val timeout = Timeout(3000)

    def receive = {
      case FindRealms(names, id) =>
        store.findRealms(names).map(r => FindRealmsResponse(r.toSet, id)) pipeTo sender
      case FindAccounts(realm, names, id) =>
        store.findAccounts(realm, names).map(s => FindAccountsResp(s.toSet, id)) pipeTo sender
      case FindAccountsFor(realm, creds, id) =>
        store.findAccountsFor(realm, creds).map(s => FindAccountsResp(s.toSet, id)) pipeTo sender
      case FindGroups(realm, names, id) =>
        store.findGroups(realm, names).map(s => FindGroupsResp(s.toSet, id)) pipeTo sender
      case GetAllRealms(id) =>
        store.allRealms.map(r => FindRealmsResponse(r.toSet, id)) pipeTo sender
      case GetAllAccounts(realm, id) =>
        store.allAccounts(realm).map(a => FindAccountsResp(a.toSet, id)) pipeTo sender
      case GetAllGroups(realm, id) =>
        store.allGroups(realm).map(g => FindGroupsResp(g.toSet, id)) pipeTo sender

      case _ => sender ! Unknown
    }
  }

  trait StoreMessage extends PorterMessage
  case class FindRealms(names: Set[Ident], id: Int = 0) extends StoreMessage
  case class GetAllRealms(id: Int = 0) extends StoreMessage
  case class FindRealmsResponse(realms: Set[Realm], id: Int) extends StoreMessage

  case class FindAccounts(realm: Ident, names: Set[Ident], id: Int = 0) extends StoreMessage
  case class FindAccountsFor(realm: Ident, creds: Set[Credentials], id: Int = 0) extends StoreMessage
  case class GetAllAccounts(realm: Ident, id: Int = 0) extends StoreMessage
  case class FindAccountsResp(accounts: Set[Account], id: Int) extends StoreMessage

  case class FindGroups(realm: Ident, names: Set[Ident], id: Int = 0) extends StoreMessage
  case class GetAllGroups(realm: Ident, id: Int = 0) extends StoreMessage
  case class FindGroupsResp(groups: Set[Group], id: Int) extends StoreMessage

}
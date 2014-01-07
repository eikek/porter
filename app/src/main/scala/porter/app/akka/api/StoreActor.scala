package porter.app.akka.api

import akka.actor.{Status, ActorRef, Props, Actor}
import porter.store.Store
import akka.util.Timeout
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future

class StoreActor(stores: List[Store]) extends Actor {
  import StoreActor.messages._
  import StoreActor.readOnlyProps

  val readonly = stores.map(s => context.actorOf(readOnlyProps(s)))

  if (readonly.isEmpty) context.become(empty)

  def receive = normal

  def empty: Receive = {
    case sm: StoreMessage => sender ! Status.Failure(new Exception("No stores provided."))
  }

  def normal: Receive = {
    case req: FindRealms => receiveRealms(sender, req)
    case req: FindAccounts => receiveAccounts(sender, req)
    case req: FindAccountsFor => receiveAccounts(sender, req)
    case req: FindGroups => receiveGroups(sender, req)
    case req: GetAllGroups => receiveGroups(sender, req)
    case req: GetAllAccounts => receiveAccounts(sender, req)
    case GetAllRealms => receiveRealms(sender, GetAllRealms)
  }

  private def receiveRealms(client: ActorRef, req: PorterMessage) {
    context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
      type Res = FindRealmsResp
      def empty = FindRealmsResp(Set())
      def merge(r1: Res, r2: Res) = FindRealmsResp(r1.realms ++ r2.realms)
      object Extr {
        def unapply(a: Any) = a match {
          case r: FindRealmsResp => Some(r)
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
      val empty = FindAccountsResp(Set())

      def merge(r1: FindAccountsResp, r2: FindAccountsResp) =
        FindAccountsResp(r1.accounts ++ r2.accounts)
    }))
  }

  private def receiveGroups(client: ActorRef, req: PorterMessage) {
    context.actorOf(Props[CollectingActor](new CollectingActor(client, req, readonly) {
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
    }))
  }

}

object StoreActor {
  import porter.model._
  import messages._

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

  object messages {
    type StoreMessage = porter.client.Messages.store.StoreMessage

    type FindRealms = porter.client.Messages.store.FindRealms
    val FindRealms = porter.client.Messages.store.FindRealms
    type FindRealmsResp = porter.client.Messages.store.FindRealmsResp
    val FindRealmsResp = porter.client.Messages.store.FindRealmsResp
    case object GetAllRealms extends StoreMessage

    val FindAccounts = porter.client.Messages.store.FindAccounts
    type FindAccounts = porter.client.Messages.store.FindAccounts
    case class FindAccountsFor(realm: Ident, creds: Set[Credentials]) extends StoreMessage
    case class GetAllAccounts(realm: Ident) extends StoreMessage
    type FindAccountsResp = porter.client.Messages.store.FindAccountsResp
    val FindAccountsResp = porter.client.Messages.store.FindAccountsResp

    type FindGroups = porter.client.Messages.store.FindGroups
    val FindGroups = porter.client.Messages.store.FindGroups
    case class GetAllGroups(realm: Ident) extends StoreMessage
    type FindGroupsResp = porter.client.Messages.store.FindGroupsResp
    val FindGroupsResp = porter.client.Messages.store.FindGroupsResp
  }

}
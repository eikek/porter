package porter.app.akka.api

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Status, ActorSystem}
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import com.typesafe.config.ConfigFactory
import porter.model.{Ident, Account, Realm}
import porter.app.akka.Porter
import porter.store.SimpleStore
import scala.concurrent.ExecutionContext

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 05.12.13 16:19
 */
class StoreActorSpec extends TestKit(ActorSystem("StoreActorSpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  import Porter.Messages.store._
  override def afterAll() {
    system.shutdown()
  }

  import TestData._

  "A StoreActor" must {

    "return realms from all stores" in {
      val store = system.actorOf(StoreActor(List(store1, store2)))
      store ! FindRealms(Set("r1", "r4"))
      expectMsg(FindRealmsResp(Set(realm, Realm("r4", "")), 0))

      store ! GetAllRealms(2)
      expectMsg(FindRealmsResp(Set(realm, Realm("r4", "")), 2))
    }

    "return accounts from all stores" in {
      val store = system.actorOf(StoreActor(List(store1, store2)))
      store ! FindAccounts(realm.id, Set("john", "gloria"))
      expectMsg(FindAccountsResp(Set(john, Account(name = "gloria")), 0))
    }

    "return groups from all stores" in {
      val store = system.actorOf(StoreActor(List(store1, store2)))
      store ! FindGroups("r1", Set("g4", "g2"))
      expectMsg(FindGroupsResp(Set(g2, g4), 0))

      store ! GetAllGroups("r1", 1)
      expectMsg(FindGroupsResp(Set(g1, g2, g3, g4), 1))
    }

    "recover from errors in store" in {
      val store = system.actorOf(StoreActor(List(new SimpleStore {
        def accounts = sys.error("fail")
        def realms = sys.error("fail")
        def groups = sys.error("fail")

        override def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) = sys.error("failure")
      })))
      store ! FindGroups("r1", Set("g2"))
      expectMsgClass(classOf[Status.Failure])

      store ! FindRealms(Set("r1"))
      expectMsgClass(classOf[Status.Failure])
    }
  }
}

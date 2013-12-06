package porter.app.akka.api

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import porter.app.akka.api.StoreActor._
import com.typesafe.config.ConfigFactory
import porter.model.Account
import porter.app.akka.api.StoreActor.FindRealmsResponse
import porter.model.Realm
import porter.app.akka.api.StoreActor.FindRealms
import porter.app.akka.api.StoreActor.FindAccountsResp
import porter.app.akka.api.StoreActor.FindAccounts

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 05.12.13 16:19
 */
class StoreActorSpec extends TestKit(ActorSystem("StoreActorSpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  override def afterAll() {
    system.shutdown()
  }

  import TestData._

  "A StoreActor" must {

    "return realms from all stores" in {
      val store = system.actorOf(StoreActor.props(List(store1, store2)))
      store ! FindRealms(Set("r1", "r4"))
      expectMsg(FindRealmsResponse(Set(realm, Realm("r4", "")), 0))

      store ! GetAllRealms(2)
      expectMsg(FindRealmsResponse(Set(realm, Realm("r4", "")), 2))
    }

    "return accounts from all stores" in {
      val store = system.actorOf(StoreActor.props(List(store1, store2)))
      store ! FindAccounts(realm.id, Set("john", "gloria"))
      expectMsg(FindAccountsResp(Set(john, Account(name = "gloria")), 0))
    }

    "return groups from all stores" in {
      val store = system.actorOf(StoreActor.props(List(store1, store2)))
      store ! FindGroups("r1", Set("g4", "g2"))
      expectMsg(FindGroupsResp(Set(g2, g4), 0))

      store ! GetAllGroups("r1", 1)
      expectMsg(FindGroupsResp(Set(g1, g2, g3, g4), 1))
    }
  }
}

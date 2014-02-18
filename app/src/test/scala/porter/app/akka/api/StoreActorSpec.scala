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

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Status, ActorSystem}
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import com.typesafe.config.ConfigFactory
import porter.model.{Ident, Account, Realm}
import porter.store.SimpleStore
import scala.concurrent.ExecutionContext
import porter.app.akka.api.StoreActor.GetAllRealms
import porter.client.Messages.store._

class StoreActorSpec extends TestKit(ActorSystem("StoreActorSpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  override def afterAll() {
    system.shutdown()
  }

  import TestData._

  "A StoreActor" must {

    "return realms from all stores" in {
      val store = system.actorOf(StoreActor(List(store1, store2)))
      store ! FindRealms(Set("r1", "r4"))
      expectMsg(FindRealmsResp(Set(realm, Realm("r4", ""))))

      store ! GetAllRealms
      expectMsg(FindRealmsResp(Set(realm, Realm("r4", ""))))
    }

    "return accounts from all stores" in {
      val store = system.actorOf(StoreActor(List(store1, store2)))
      store ! FindAccounts(realm.id, Set("john", "gloria"))
      expectMsg(FindAccountsResp(Set(john, Account(name = "gloria"))))
    }

    "return groups from all stores" in {
      val store = system.actorOf(StoreActor(List(store1, store2)))
      store ! FindGroups("r1", Set("g4", "g2"))
      expectMsg(FindGroupsResp(Set(g2, g4)))

      store ! GetAllGroups("r1")
      expectMsg(FindGroupsResp(Set(g1, g2, g3, g4)))
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

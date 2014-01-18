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
import akka.actor.{Status, Props, ActorSystem}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import porter.model.{Realm, Ident}
import porter.app.akka.api.MutableStoreActor.messages.{OperationFinished, UpdateRealm}
import scala.concurrent.{Future, ExecutionContext}

class MutableStoreSpec extends TestKit(ActorSystem("RuleFactoryActorSpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  import TestData._
  import scala.concurrent.duration._

  override def afterAll() = {
    system.shutdown()
  }

  "A mutable store" must {

    "kill the working actor after it is done" in {
      val store = system.actorOf(Props[MutableStoreActor](new MutableStoreActor(List(Set.empty[Ident] -> new EmptyMutableStore)) {
        override def receive = super.receive orElse {
          case "count" => sender ! context.children.size
        }
      }))
      store ! "count"
      expectMsg(0)

      store ! UpdateRealm(realm)
      expectMsg(OperationFinished(result = true))
      this.expectNoMsg(300.millis)
      store ! "count"
      expectMsg(0)
    }

    "recover from store future error" in {
      val failstore = new EmptyMutableStore {
        override def updateRealm(realm: Realm)(implicit ec: ExecutionContext) = Future.failed(new Exception())
      }
      val store = system.actorOf(MutableStoreActor(List(Set.empty[Ident] -> failstore)))

      store ! UpdateRealm(realm)
      expectMsg(OperationFinished(result = false))
    }

    "recover from store error" in {
      val failstore = new EmptyMutableStore {
        override def updateRealm(realm: Realm)(implicit ec: ExecutionContext) = sys.error("failure")
      }
      val store = system.actorOf(MutableStoreActor(List(Set.empty[Ident] -> failstore)))

      store ! UpdateRealm(realm)
      expectMsg(OperationFinished(result = false))
    }
  }
}

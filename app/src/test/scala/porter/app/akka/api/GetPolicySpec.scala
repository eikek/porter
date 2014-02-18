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
import akka.actor.{Props, ActorSystem}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import porter.auth.RuleFactory
import porter.model.Policy
import porter.app.akka.api.PolicyActor.{GetPolicyResp, GetPolicy}

class GetPolicySpec extends TestKit(ActorSystem("GetPolicySpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  import porter.client.Messages.auth._
  override def afterAll() {
    system.shutdown()
  }

  import TestData._

  def createActor() = {
    val factory = system.actorOf(RuleFactoryActor(Vector(RuleFactory.providedFactory)))
    val store = system.actorOf(StoreActor(List(store1, store2)))
    system.actorOf(Props(classOf[PolicyActor], store, factory))
  }

  "A GetPolicyActor" must {

    "return reduced policy" in {
      val actor = createActor()
      actor ! GetPolicy(realm.id, john.name)
      expectMsg(GetPolicyResp("john", policyJohn))
    }

    "return empty policy for unknown accounts" in {
      val actor = createActor()
      actor ! GetPolicy(realm.id, "heinz")
      expectMsg(GetPolicyResp("heinz", Policy.empty))
    }
  }
}

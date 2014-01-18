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

import org.scalatest.{BeforeAndAfterAll, WordSpec}
import akka.actor.{Status, Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import porter.auth.RuleFactory
import porter.model.{ResourcePermission, DefaultPermission}
import com.typesafe.config.ConfigFactory
import porter.app.akka.Porter

class RuleFactoryActorSpec extends TestKit(ActorSystem("RuleFactoryActorSpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  import Porter.Messages.rules._
  override def afterAll() {
    system.shutdown()
  }

  "A RuleFactory" must {

    "return the correct permissions" in {
      val factory = system.actorOf(RuleFactoryActor(Vector(RuleFactory.providedFactory)))
      factory ! MakeRules(Set("git:push:*", "resource:read:/main/**", "resource:read:/index"))
      expectMsg(MakeRulesResp(Set(
        DefaultPermission("git:push:*"),
        ResourcePermission("resource:read:/main/**"),
        ResourcePermission("resource:read:/index")
      )))
    }

    "return failure status in case of error" in {
      val factory = system.actorOf(RuleFactoryActor(Vector(RuleFactory.providedFactory)))
      factory ! MakeRules(Set(""))
      expectMsgType[Status.Failure]
    }

    "return Unknown message for other requests" in {
      val factory = system.actorOf(RuleFactoryActor(Vector(RuleFactory.providedFactory)))
      factory ! "bla"
      expectMsg(Unknown)
    }
  }
}

package porter.app.akka.api

import org.scalatest.{BeforeAndAfterAll, WordSpec}
import akka.actor.{Status, Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import porter.auth.RuleFactory
import porter.app.akka.PorterActor.{MakeRulesResponse, MakeRules}
import porter.model.{ResourcePermission, DefaultPermission}

/**
 * @since 05.12.13 13:59
 */
class RuleFactoryActorSpec extends TestKit(ActorSystem("RuleFactoryActorSpec"))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  override def afterAll() {
    system.shutdown()
  }

  "A RuleFactory" must {

    "return the correct permissions" in {
      val factory = system.actorOf(Props[RuleFactoryActor](new RuleFactoryActor(Vector(RuleFactory.providedFactory))))
      factory ! MakeRules(Set("git:push:*", "resource:read:/main/**", "resource:read:/index"))
      expectMsg(MakeRulesResponse(Set(
        DefaultPermission("git:push:*"),
        ResourcePermission("resource:read:/main/**"),
        ResourcePermission("resource:read:/index")
      )))
    }

    "return failure status in case of error" in {
      val factory = system.actorOf(Props[RuleFactoryActor](new RuleFactoryActor(Vector(RuleFactory.providedFactory))))
      factory ! MakeRules(Set(""))
      expectMsgType[Status.Failure]
    }

    "return Unknown message for other requests" in {
      val factory = system.actorOf(Props[RuleFactoryActor](new RuleFactoryActor(Vector(RuleFactory.providedFactory))))
      factory ! "bla"
      expectMsg(Unknown)
    }
  }
}

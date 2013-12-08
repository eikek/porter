package porter.app.akka.api

import org.scalatest.{BeforeAndAfterAll, WordSpec}
import akka.actor.{Status, Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import porter.auth.RuleFactory
import porter.model.{ResourcePermission, DefaultPermission}
import com.typesafe.config.ConfigFactory
import porter.app.akka.Porter

/**
 * @since 05.12.13 13:59
 */
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
      ), 0))
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

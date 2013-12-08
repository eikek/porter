package porter.app.akka.api

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Props, ActorSystem}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import porter.auth.RuleFactory
import porter.model.Policy
import porter.app.akka.Porter

class GetPolicySpec extends TestKit(ActorSystem("GetPolicySpec", ConfigFactory.load("reference")))
  with WordSpec with BeforeAndAfterAll with ImplicitSender {

  import Porter.Messages.authz._
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
      actor ! GetPolicy(realm.id, john.name, 1)
      expectMsg(GetPolicyResp("john", policyJohn, 1))
    }

    "return empty policy for unknown accounts" in {
      val actor = createActor()
      actor ! GetPolicy(realm.id, "heinz", 2)
      expectMsg(GetPolicyResp("heinz", Policy.empty, 2))
    }
  }
}

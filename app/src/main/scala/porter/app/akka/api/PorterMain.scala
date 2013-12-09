package porter.app.akka.api

import akka.actor.{Props, Actor}
import porter.auth.RuleFactory
import porter.app.PorterSettings
import porter.app.akka.api.PorterMain.ShowSettings
import porter.app.akka.api.StoreActor.messages.StoreMessage
import porter.app.akka.api.RuleFactoryActor.messages.MakeRules
import porter.app.akka.api.MutableStoreActor.messages.MutableStoreMessage
import porter.app.akka.api.PolicyActor.messages.{Authorize, GetPolicy}
import porter.app.akka.api.AuthcWorker.messages.Authenticate

class PorterMain(settings: PorterSettings) extends Actor {

  val store = context.actorOf(StoreActor(settings.stores), name = "porter-store")
  val mstore = context.actorOf(MutableStoreActor(settings.mutableStores), name = "porter-mutablestore")
  val ruleFactory = context.actorOf(RuleFactoryActor(
    settings.permissionFactories :+ RuleFactory.providedFactory), name = "porter-rulefactory")
  val policy = context.actorOf(PolicyActor(store, ruleFactory), name = "porter-policy")

  def receive = {
    case s: ShowSettings => sender ! settings.toString
    case sm: StoreMessage => store forward sm
    case mm: MutableStoreMessage => mstore forward mm
    case mr: MakeRules => ruleFactory forward mr
    case gp: GetPolicy => policy forward gp
    case authz: Authorize => policy forward authz
    case authc: Authenticate =>
      val w = context.actorOf(AuthcWorker(store, settings.validators))
      w forward authc
  }
}

object PorterMain {

  case class ShowSettings(id: Int = 0) extends PorterMessage

  def apply(settings: PorterSettings) = Props(classOf[PorterMain], settings)
}
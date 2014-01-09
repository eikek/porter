package porter.app.akka.api

import akka.actor.{ActorLogging, Props, Actor}
import porter.auth.RuleFactory
import porter.app.PorterSettings
import porter.app.akka.api.PorterMain.ShowSettings
import porter.app.akka.api.StoreActor.messages.StoreMessage
import porter.app.akka.api.RuleFactoryActor.messages.MakeRules
import porter.app.akka.api.MutableStoreActor.messages._
import porter.app.akka.api.PolicyActor.messages.{Authorize, GetPolicy}
import porter.app.akka.api.AuthcWorker.messages.Authenticate

class PorterMain(settings: PorterSettings) extends Actor with ActorLogging {

  val store = context.actorOf(StoreActor(settings.stores), name = "porter-store")
  val mstore = context.actorOf(MutableStoreActor(settings.mutableStores), name = "porter-mutablestore")
  val ruleFactory = context.actorOf(RuleFactoryActor(
    settings.permissionFactories :+ RuleFactory.providedFactory), name = "porter-rulefactory")
  val policy = context.actorOf(PolicyActor(store, ruleFactory), name = "porter-policy")
  val extras = context.actorOf(ExtrasActor(store, mstore, ruleFactory, policy), name = "porter-extras")

  def receive = {
    case ShowSettings => sender ! settings.toString
    case sm: StoreMessage => store forward sm
    case mm: MutableStoreMessage => mstore forward mm
    case mr: MakeRules => ruleFactory forward mr
    case gp: GetPolicy => policy forward gp
    case authz: Authorize => policy forward authz
    case authc: Authenticate =>
      val w = context.actorOf(AuthcWorker(store, settings.validators))
      w forward authc
    case rest: PorterMessage => extras forward rest
  }
}

object PorterMain {
  case object ShowSettings extends PorterMessage

  def apply(settings: PorterSettings) = Props(classOf[PorterMain], settings)
}
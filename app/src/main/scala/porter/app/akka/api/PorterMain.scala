package porter.app.akka.api

import akka.actor.Actor
import porter.auth.RuleFactory
import porter.app.akka.api.StoreActor.StoreMessage
import porter.app.akka.api.MutableStoreActor.MutableStoreMessage
import porter.app.akka.api.RuleFactoryActor.MakeRules
import porter.app.akka.api.PolicyActor.{Authorize, GetPolicy}
import porter.app.akka.api.AuthcWorker.Authenticate
import porter.app.PorterSettings
import porter.app.akka.api.PorterMain.ShowSettings

class PorterMain(settings: PorterSettings) extends Actor {

  val store = context.actorOf(StoreActor.props(settings.stores), name ="porter-store")
  val mstore = context.actorOf(MutableStoreActor.props(settings.mutableStores), name = "porter-mutablestore")
  val ruleFactory = context.actorOf(RuleFactoryActor.props(settings.permissionFactories :+ RuleFactory.providedFactory),
    name = "porter-rulefactory")
  val policy = context.actorOf(PolicyActor.props(store, ruleFactory), name = "porter-policy")

  def receive = {
    case sm: StoreMessage => store forward sm
    case mm: MutableStoreMessage => mstore forward mm
    case mr: MakeRules => ruleFactory forward mr
    case gp: GetPolicy => policy forward gp
    case s: ShowSettings => sender ! settings.toString
    case authz: Authorize => policy forward authz
    case authc: Authenticate =>
      val w = context.actorOf(AuthcWorker.workerProps(store, settings.authenticators))
      w forward authc
  }
}

object PorterMain {

  case class ShowSettings(id: Int = 0) extends PorterMessage
}
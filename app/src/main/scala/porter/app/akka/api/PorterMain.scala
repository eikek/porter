package porter.app.akka.api

import akka.actor.{ActorLogging, Props, Actor}
import porter.auth.RuleFactory
import porter.app.PorterSettings
import porter.app.akka.api.PorterMain.{UpdateAuthProps, ShowSettings}
import porter.app.akka.api.StoreActor.messages.{FindAccountsResp, FindAccountsFor, StoreMessage}
import porter.app.akka.api.RuleFactoryActor.messages.MakeRules
import porter.app.akka.api.MutableStoreActor.messages._
import porter.app.akka.api.PolicyActor.messages.{Authorize, GetPolicy}
import porter.app.akka.api.AuthcWorker.messages.Authenticate
import porter.model.Credentials
import akka.util.Timeout

class PorterMain(settings: PorterSettings) extends Actor with ActorLogging {

  val store = context.actorOf(StoreActor(settings.stores), name = "porter-store")
  val mstore = context.actorOf(MutableStoreActor(settings.mutableStores), name = "porter-mutablestore")
  val ruleFactory = context.actorOf(RuleFactoryActor(
    settings.permissionFactories :+ RuleFactory.providedFactory), name = "porter-rulefactory")
  val policy = context.actorOf(PolicyActor(store, ruleFactory), name = "porter-policy")

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
    case UpdateAuthProps(realm, creds, success) =>
      import porter.model.PropertyList._
      import akka.pattern.ask
      import akka.pattern.pipe
      import context.dispatcher
      implicit val timeout = Timeout(5000)
      lazy val props =
        if (success) lastLoginTime.current.andThen(successfulLogins.increment)
        else failedLogins.increment
      val f = for {
        acc <- (store ? FindAccountsFor(realm, creds)).mapTo[FindAccountsResp]
        if acc.accounts.nonEmpty
        upd <- (mstore ? UpdateAccount(realm, acc.accounts.head.updatedProps(props))).mapTo[OperationFinished]
      } yield upd
      f.onFailure { case x =>
        log.error(x, "Cannot update acount properties")
        OperationFinished(result = false)
      }
      f pipeTo sender
  }
}

object PorterMain {
  import porter.model.Ident

  case object ShowSettings extends PorterMessage
  case class UpdateAuthProps(realm: Ident, creds: Set[Credentials], success: Boolean) extends PorterMessage

  def apply(settings: PorterSettings) = Props(classOf[PorterMain], settings)
}
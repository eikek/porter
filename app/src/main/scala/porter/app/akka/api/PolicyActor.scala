package porter.app.akka.api

import akka.actor.{Status, Props, ActorRef, Actor}
import porter.app.akka.api.StoreActor.{FindGroupsResp, FindAccountsResp}
import porter.app.akka.api.RuleFactoryActor.{MakeRulesResponse, MakeRules}

class PolicyActor(store: ActorRef, ruleFactory: ActorRef) extends Actor {
  import PolicyActor._
  import porter.model._
  import akka.actor.ActorDSL._

  def receive = {
    case req: GetPolicy=> findPolicy(sender, req)
    case req: Authorize => authorize(sender, req)
  }

  def findPolicy(client: ActorRef, req: GetPolicy) = actor(new Act {
    whenStarting { store ! StoreActor.FindAccounts(req.realmId, Set(req.account), req.id) }
    become {
      case FindAccountsResp(accounts, id) =>
        if (accounts.isEmpty || accounts.size > 1) {
          client ! GetPolicyResp(req.account, Policy.empty, id)
          context.stop(self)
        } else {
          store ! StoreActor.FindGroups(req.realmId, accounts.flatMap(_.groups), id)
        }
      case FindGroupsResp(groups, id) =>
        if (groups.isEmpty) {
          client ! GetPolicyResp(req.account, Policy.empty, id)
          context.stop(self)
        } else {
          ruleFactory ! MakeRules(groups.flatMap(_.rules), id)
        }
      case MakeRulesResponse(rules, id) =>
        val policy = Policy(rules)
        client ! GetPolicyResp(req.account, policy, id)
        context.stop(self)
    }
  })

  def authorize(client: ActorRef, req: Authorize) = actor(new Act {
    var policy: Option[Policy] = None
    var rules: Option[Set[Rule]] = None

    whenStarting {
      ruleFactory ! MakeRules(req.perms.toSet, req.id)
      findPolicy(self, GetPolicy(req.realm, req.account, req.id))
    }

    become {
      case GetPolicyResp(_, p, _) =>
        this.policy = Some(p)
        if (rules.nonEmpty) self ! "DONE"
      case MakeRulesResponse(r, _) =>
        this.rules = Some(r)
        if (policy.nonEmpty) self ! "DONE"
      case "DONE" =>
        val (perms, rev) = partitionRules(rules.get)
        if (rev.nonEmpty) {
          client ! Status.Failure(new Exception("Cannot authorize revocations: "+rev))
        } else {
          client ! AuthorizeResp(req.realm, req.account, policy.get.grantsAll(perms), req.id)
        }
        context.stop(self)
    }
  })

}

object PolicyActor {
  import porter.model._

  case class GetPolicy(realmId: Ident, account: Ident, id: Int = 0) extends RealmMessage
  case class GetPolicyResp(account: Ident, policy: Policy, id: Int) extends PorterMessage

  case class Authorize(realm: Ident, account: Ident, perms: Iterable[String], id: Int = 0) extends PorterMessage
  case class AuthorizeResp(realm: Ident, account: Ident, authorized: Boolean, id: Int) extends PorterMessage

  def props(store: ActorRef, ruleFactory: ActorRef) =
    Props(classOf[PolicyActor], store, ruleFactory)
}

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

import akka.actor.{Status, Props, ActorRef, Actor}
import porter.client.Messages.auth._
import porter.client.Messages.store._
import porter.model._

class PolicyActor(store: ActorRef, ruleFactory: ActorRef) extends Actor {
  import PolicyActor._
  import RuleFactoryActor._
  import akka.actor.ActorDSL._

  private case object Done

  def receive = {
    case req: GetPolicy=> findPolicy(sender, req)
    case req: Authorize => authorize(sender, req)
  }

  def findPolicy(client: ActorRef, req: GetPolicy) = actor(new Act {
    whenStarting { store ! FindAccounts(req.realmId, Set(req.account)) }
    become {
      case FindAccountsResp(accounts) =>
        if (accounts.isEmpty || accounts.size > 1) {
          client ! GetPolicyResp(req.account, Policy.empty)
          context.stop(self)
        } else {
          store ! FindGroups(req.realmId, accounts.flatMap(_.groups))
        }
      case FindGroupsResp(groups) =>
        if (groups.isEmpty) {
          client ! GetPolicyResp(req.account, Policy.empty)
          context.stop(self)
        } else {
          ruleFactory ! MakeRules(groups.flatMap(_.rules))
        }
      case MakeRulesResp(rules) =>
        val policy = Policy(rules)
        client ! GetPolicyResp(req.account, policy)
        context.stop(self)

      case err: Status.Failure =>
        client ! err
        context.stop(self)
    }
  })

  def authorize(client: ActorRef, req: Authorize) = actor(new Act {
    var policy: Option[Policy] = None
    var rules: Option[Set[Rule]] = None

    whenStarting {
      ruleFactory ! MakeRules(req.perms.toSet)
      findPolicy(self, GetPolicy(req.realm, req.account))
    }

    become {
      case GetPolicyResp(_, p) =>
        this.policy = Some(p)
        if (rules.nonEmpty) self ! Done
      case MakeRulesResp(r) =>
        this.rules = Some(r)
        if (policy.nonEmpty) self ! Done
      case Done =>
        val (perms, rev) = Rules.partition(rules.get)
        if (rev.nonEmpty) {
          client ! Status.Failure(new Exception("Cannot authorize revocations: "+rev))
        } else {
          client ! AuthorizeResp(req.realm, req.account, policy.get.grantsAll(perms))
        }
        context.stop(self)
      case err: Status.Failure =>
        client ! err
        context.stop(self)
    }
  })

}

object PolicyActor {
  import porter.model._

  case class GetPolicy(realmId: Ident, account: Ident) extends PorterMessage
  case class GetPolicyResp(account: Ident, policy: Policy) extends PorterMessage

  def apply(store: ActorRef, ruleFactory: ActorRef) =
    Props(classOf[PolicyActor], store, ruleFactory)
}

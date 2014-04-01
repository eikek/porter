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

import akka.actor.{Terminated, ActorLogging, Props, Actor}
import porter.auth.RuleFactory
import porter.app.PorterSettings
import porter.app.akka.api.PorterMain.ShowSettings
import porter.client.messages._
import porter.app.akka.api.RuleFactoryActor.MakeRules
import porter.app.akka.api.PolicyActor.GetPolicy

class PorterMain(settings: PorterSettings) extends Actor with ActorLogging {

  val store = context.actorOf(StoreActor(settings.stores), name = "porter-store")
  val mstore = context.actorOf(MutableStoreActor(settings.mutableStores), name = "porter-mutablestore")
  val ruleFactory = context.actorOf(RuleFactoryActor(
    settings.permissionFactories :+ RuleFactory.providedFactory), name = "porter-rulefactory")
  val policy = context.actorOf(PolicyActor(store, ruleFactory), name = "porter-policy")
  val extras = context.actorOf(ExtrasActor(store, mstore, ruleFactory, policy), name = "porter-extras")

  var authcCreated = 0
  var authcActive = 0

  def receive = {
    case ShowSettings => sender ! settings.toString
    case sm: StoreMessage => store forward sm
    case mm: MutableStoreMessage => mstore forward mm
    case mr: MakeRules => ruleFactory forward mr
    case gp: GetPolicy => policy forward gp
    case authz: Authorize => policy forward authz
    case authc: Authenticate =>
      val w = context.watch(context.actorOf(AuthcWorker(store, settings.validators), name = s"authc$authcCreated"))
      authcCreated += 1; authcActive += 1
      w forward authc
    case rest: PorterMessage => extras forward rest
    case Terminated(ref) =>
      authcActive -= 1
      log.debug(s"Actor $ref terminated. Active workers left: $authcActive")
  }
}

object PorterMain {
  case object ShowSettings extends PorterMessage

  def apply(settings: PorterSettings) = Props(classOf[PorterMain], settings)
}
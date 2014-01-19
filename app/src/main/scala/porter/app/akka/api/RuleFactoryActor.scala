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

import akka.actor.{Status, Props, Actor}
import porter.model._
import porter.auth.RuleFactory
import scala.util.{Failure, Try, Success}
import scala.Some

class RuleFactoryActor(list: Iterable[PermissionFactory]) extends Actor {
  import RuleFactoryActor.messages._

  lazy val factory = RuleFactory.createRuleWith(list.head)_
  val next =
    if (list.tail.nonEmpty) Some(context.actorOf(Props(classOf[RuleFactoryActor], list.tail)))
    else None

  val isLast = next.isEmpty

  if (list.isEmpty) context.become(empty)

  def makeRules(str: Set[String]) = Try(str.map(factory andThen (_.get)))

  def receive = normal

  def empty: Receive = {
    case MakeRules(_) =>
      sender ! Status.Failure(new Exception("No permission factories provided."))
  }

  def normal: Receive = {
    case req@MakeRules(rules) =>
      makeRules(rules) match {
        case Success(s) if s.nonEmpty || isLast =>
          sender ! MakeRulesResp(s)
        case Success(s) if s.isEmpty =>
          next map (_ forward req)
        case Failure(ex) =>
          sender ! Status.Failure(ex)
      }

    case _ => sender ! Unknown
  }

}

object RuleFactoryActor {

  def apply(list: Iterable[PermissionFactory]) = {
    Props(classOf[RuleFactoryActor], list)
  }

  object messages {
    case class MakeRules(rules: Set[String]) extends PorterMessage
    case class MakeRulesResp(rules: Set[Rule]) extends PorterMessage {
      lazy val (permissions, revocations) = Rules.partition(rules)
    }
  }

}
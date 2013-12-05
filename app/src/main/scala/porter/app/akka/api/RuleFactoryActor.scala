package porter.app.akka.api

import akka.actor.{Status, Props, Actor}
import porter.model.{Rule, PermissionFactory}
import porter.app.akka.PorterActor.{MakeRulesResponse, MakeRules}
import porter.auth.RuleFactory
import scala.util.{Failure, Try, Success}

/**
 * @since 05.12.13 00:55
 */
class RuleFactoryActor(list: Vector[PermissionFactory]) extends Actor {
  
  val factory = RuleFactory.createRuleWith(list.head)_
  val next =
    if (list.tail.nonEmpty) Some(context.actorOf(Props(classOf[RuleFactoryActor], list.tail)))
    else None

  val isLast = next.isEmpty

  def makeRules(str: Set[String]) = Try(str.map(factory andThen (_.get)))

  def receive = {
    case req@MakeRules(rules) =>
      makeRules(rules) match {
        case Success(s) if s.nonEmpty || isLast =>
          sender ! MakeRulesResponse(s)
        case Success(s) if s.isEmpty =>
          next map (_ forward req)
        case Failure(ex) =>
          sender ! Status.Failure(ex)
      }

    case _ => sender ! Unknown
  }

}

object RuleFactoryActor {

}
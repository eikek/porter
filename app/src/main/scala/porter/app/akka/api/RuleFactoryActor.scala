package porter.app.akka.api

import akka.actor.{Status, Props, Actor}
import porter.model._
import porter.auth.RuleFactory
import scala.util.{Failure, Try, Success}
import scala.Some

/**
 * @since 05.12.13 00:55
 */
class RuleFactoryActor(list: Iterable[PermissionFactory]) extends Actor {
  import RuleFactoryActor._

  val factory = RuleFactory.createRuleWith(list.head)_
  val next =
    if (list.tail.nonEmpty) Some(context.actorOf(Props(classOf[RuleFactoryActor], list.tail)))
    else None

  val isLast = next.isEmpty

  def makeRules(str: Set[String]) = Try(str.map(factory andThen (_.get)))

  def receive = {
    case req@MakeRules(rules, id) =>
      makeRules(rules) match {
        case Success(s) if s.nonEmpty || isLast =>
          sender ! MakeRulesResponse(s, id)
        case Success(s) if s.isEmpty =>
          next map (_ forward req)
        case Failure(ex) =>
          sender ! Status.Failure(ex)
      }

    case _ => sender ! Unknown
  }

}

object RuleFactoryActor {

  def props(list: Iterable[PermissionFactory]) = {
    require(list.nonEmpty, "empty permission factory list not allowed")
    Props(classOf[RuleFactoryActor], list)
  }

  case class MakeRules(rules: Set[String], id: Int = 0) extends PorterMessage

  case class MakeRulesResponse(rules: Set[Rule], id: Int) extends PorterMessage {
    lazy val (permissions, revocations) = partitionRules(rules)
  }
}
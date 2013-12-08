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
    case MakeRules(_, _) =>
      sender ! Status.Failure(new Exception("No permission factories provided."))
  }

  def normal: Receive = {
    case req@MakeRules(rules, id) =>
      makeRules(rules) match {
        case Success(s) if s.nonEmpty || isLast =>
          sender ! MakeRulesResp(s, id)
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
    case class MakeRules(rules: Set[String], id: Int = 0) extends PorterMessage
    case class MakeRulesResp(rules: Set[Rule], id: Int) extends PorterMessage {
      lazy val (permissions, revocations) = partitionRules(rules)
    }
  }

}
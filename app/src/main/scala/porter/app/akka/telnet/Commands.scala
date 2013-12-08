package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import scala.util.{Try, Failure, Success}
import akka.actor.ActorRef

/**
 *
 * @since 25.11.13 09:47
 *
 */
trait Commands {

  def make(implicit executor: ExecutionContext, to: Timeout): Seq[Command]

  def ++ (other: Commands) = {
    val self = this
    new Commands {
      def make(implicit executor: ExecutionContext, to: Timeout) = self.make ++ other.make
    }
  }

  def reduce(implicit executor: ExecutionContext, to: Timeout): Command =
    (make :+ notFound) reduce (_ orElse _)

  private def notFound: Command = {
    case in@Input(msg, _, _, _) => in << s"Command '$msg' not found"
  }
}

object Commands {

  def makePairs(str: String): Try[Map[String, String]] = Try {
    str.split(',').withFilter(s => s.trim.nonEmpty).map { s =>
      s.split('=').toList match {
        case k::v::Nil => k.trim -> v.trim
        case _ => throw new IllegalArgumentException("Cannot create pair from: "+ s)
      }
    }.toMap
  }

  def makeList(sep: Char)(str: String) =
    str.split(sep).filter(s => s.trim.nonEmpty).toList

}
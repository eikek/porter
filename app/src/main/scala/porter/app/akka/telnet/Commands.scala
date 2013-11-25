package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import scala.util.{Failure, Success}
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
    case Input(msg, conn, _, _) => conn ! tcp(s"Command '$msg' not found")
  }

}

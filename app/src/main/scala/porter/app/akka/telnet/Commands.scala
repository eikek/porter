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

  def makeDoc: String

  final def ++ (other: Commands) = {
    val self = this
    new Commands {
      def make(implicit executor: ExecutionContext, to: Timeout) = self.make ++ other.make
      def makeDoc = self.makeDoc + other.makeDoc
    }
  }

  final def reduce(implicit executor: ExecutionContext, to: Timeout): Command =
    (make :+ notFound) reduce (_ orElse _)

  private def notFound: Command = {
    case in@Input(msg, _, _, _) => in << s"Command '$msg' not found"
  }

  protected def propsToString(props: porter.model.Properties, start: String, sep: String, end: String): String = {
    def cutString(str: String) = if (str.length > 80) str.substring(0, 77)+"..." else str
    props.map({case (k,v) => k +"="+ cutString(v) }).mkString(start, sep, end)
  }
}
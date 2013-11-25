package porter.app.akka

import akka.actor.ActorRef
import porter.model.Realm
import akka.io.Tcp
import akka.util.ByteString
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 *
 * @since 25.11.13 09:43
 *
 */
package object telnet {

  case class Session(var realm: Option[Realm])
  case class Input(msg: String, conn: ActorRef, porter: ActorRef, session: Session = Session(None)) {

    def andThen[A](f: Future[A])(callback: A => Unit)(implicit ec: ExecutionContext) {
      f.andThen {
        case Success(a) => callback(a)
        case Failure(x) => conn ! tcp("Error: "+ x.getMessage)
      }
    }
  }

  type Command = PartialFunction[Input, Unit]

  def tcp(s: String) = Tcp.Write(ByteString(s+"\nporter> "))
}

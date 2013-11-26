package porter.app.akka

import akka.actor.ActorRef
import porter.model.Realm
import akka.io.Tcp
import akka.util.ByteString
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Failure, Success}

/**
 *
 * @since 25.11.13 09:43
 *
 */
package object telnet {

  class Session(var realm: Option[Realm]) {
    private val attr = collection.mutable.Map.empty[Any, Any]

    def put(key: Any, a: Any) {
      this.attr.put(key, a)
    }
    def get(key: Any) = this.attr.get(key)
    def remove(key: Any) = this.attr.remove(key)
    def hasKey(key: Any) = this.attr.get(key).isDefined
    def noKey(key: Any) = !hasKey(key)
  }

  case class Input(msg: String, conn: ActorRef, porter: PorterExt, session: Session = new Session(None)) {

    def sendError: PartialFunction[Throwable, Unit] = {
      case x => conn ! prompt("Error: "+ x.getMessage)
    }

    def onSuccess[A](f: Future[A])(callback: A => Unit)(implicit ec: ExecutionContext) {
      f.map(callback).recover(sendError)
    }

    def onSuccess[A](t: Try[A])(f: A => Unit) {
      t.map(f).recover(sendError)
    }

    def withRealm(f: Realm => Unit) {
      session.realm.map(f).getOrElse {
        conn ! prompt("Error: No realm defined. Set one with 'use realm <id>'.")
      }
    }
  }

  type Command = PartialFunction[Input, Unit]

  def prompt(s: String) = Tcp.Write(ByteString(s+"\nporter> "))
  def tcp(s: String) = Tcp.Write(ByteString(s))
}

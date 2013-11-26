package porter.app.akka

import akka.actor.ActorRef
import porter.model.{Properties, Realm}
import akka.io.Tcp
import akka.util.ByteString
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Failure, Success}
import scala.reflect.ClassTag

/**
 *
 * @since 25.11.13 09:43
 *
 */
package object telnet {

  type Command = PartialFunction[Input, Unit]

  class Session(var realm: Option[Realm]) {
    private var attr = Map.empty[String, Any]
    private var _token: Option[String] = None

    def token_=(t: String) {
      this._token = Some(t)
    }
    def token = _token
    def removeToken() { this._token = None }

    def apply[A](key: String) = get(key).map(_.asInstanceOf[A])
      .getOrElse(throw new NoSuchElementException(s"No value for key '$key' in $attr"))

    def add(key: String, value: Any) {
      this.attr = this.attr.updated(key, value)
    }
    def get(k: String) = attr.collect({ case (`k`, v) => v }).headOption

    def remove(ks: String*) {
      val set = ks.toSet
      this.attr = attr.filter({case (k, _) => !set.contains(k)})
    }
  }

  case class Input(msg: String, conn: ActorRef, porter: PorterExt, session: Session = new Session(None)) {

    def sendError: PartialFunction[Throwable, Unit] = {
      case x => this << ("Error: "+ x.getMessage)
    }

    def onSuccess[A](f: Future[A])(callback: A => Unit)(implicit ec: ExecutionContext) {
      f.map(callback).recover(sendError)
    }

    def onSuccess[A](t: Try[A])(f: A => Unit) {
      t.map(f).recover(sendError)
    }

    def withRealm(f: Realm => Unit) {
      session.realm.map(f).getOrElse {
        this << "Error: No realm defined. Set one with 'use realm <id>'."
      }
    }

    def token_=(t: String) { session.token = t }
    def token = session.token
    def tokenIs(s: String): Boolean = token == Some(s)

    def << (m: String) {
      conn ! prompt(m, session.realm)
    }
  }

  def prompt(s: String, realm: Option[Realm] = None) = {
    realm match {
      case Some(r) => Tcp.Write(ByteString(s"$s\n(${r.id.name}) porter> "))
      case _ => Tcp.Write(ByteString(s+"\nporter> "))
    }
  }
  def tcp(s: String) = Tcp.Write(ByteString(s))


}

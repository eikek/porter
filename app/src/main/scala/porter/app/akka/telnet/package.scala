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

package porter.app.akka

import akka.actor.ActorRef
import porter.model.{Properties, Realm}
import akka.io.Tcp
import akka.util.ByteString
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Failure, Success}
import scala.reflect.ClassTag

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

  case class Input(msg: String, conn: ActorRef, porter: ActorRef, session: Session = new Session(None)) {

    def sendError: PartialFunction[Throwable, Unit] = {
      case x => this << ("Error: "+ x.getMessage)
    }

    def withRealm(f: Realm => Unit) {
      session.realm.map(f).getOrElse {
        this << "Error: No realm defined. Set one with 'use realm <id>'."
      }
    }

    def realmFuture = {
      import _root_.porter.util._
      Future.immediate(session.realm,
        "No realm defined. Set one with 'use realm <id>'.")
    }

    def token_=(t: String) { session.token = t }
    def token = session.token

    def <~ (m: String) {
      conn ! tcp(m)
    }
    def << (m: String) {
      conn ! prompt(m, session.realm)
    }

    def << (fm: Future[String])(implicit ec: ExecutionContext) {
      fm.map(s => this << s).recover(sendError)
    }

    def <<<[I <: Iterable[String]] (fm: Future[I])(implicit ec: ExecutionContext) {
      fm.map(is => this << is.mkString("\n")).recover(sendError)
    }
  }

  def prompt(s: String, realm: Option[Realm] = None) = {
    realm match {
      case Some(r) => Tcp.Write(ByteString(s"$s\n(${r.id.name}) porter> "))
      case _ => Tcp.Write(ByteString(s+"\nporter> "))
    }
  }
  def tcp(s: String) = Tcp.Write(ByteString(s))

  def illegalArg(m: String) = new IllegalArgumentException(m)

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

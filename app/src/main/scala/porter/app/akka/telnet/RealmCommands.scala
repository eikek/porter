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

package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import porter.model.{Ident, Realm}
import scala.util.Try
import akka.util.Timeout
import porter.app.akka.api.StoreActor.GetAllRealms

object RealmCommands extends Commands {

  import akka.pattern.ask
  import porter.util._
  import porter.client.messages._

  def makeDoc =
    """
      |Realm commands
      |--------------
      |use realm <realmId>      sets the given realm. most commands need
      |                         a realm to work. this realm can be set by
      |                         this command.
      |lr                       list all realms
      |update realm             updates an existing realm or creates a new one
      |                         if none exists with the given realm id
      |delete realm <realmId>   deletes a realm with the given id
    """.stripMargin

  def make(implicit executor: ExecutionContext, to: Timeout): Seq[Command] =
    List(changeRealm, listRealms, updateRealm, deleteRealm)

  def changeRealm(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, sess) if msg.startsWith("use realm") =>
      in << (for {
        realm <- Future.immediate(Ident(msg.substring("use realm".length).trim))
        iter <- (porter ? FindRealms(Set(realm))).mapTo[FindRealmsResp].map(_.realms)
      } yield {
        sess.realm = iter.headOption
        if (iter.headOption.isDefined) s"Using realm ${sess.realm.get.id.name}: ${sess.realm.get.name}"
        else "Realm not found"
      })
  }

  def listRealms(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg startsWith "lr" =>
      val filter = if (msg == "lr") (s: String) => true else (s: String) => Glob.matches(msg.substring(3), s)
      for (iter <- (porter ? GetAllRealms).mapTo[FindRealmsResp].map(_.realms)) {
        for (r <- iter; if filter(r.id.name) || filter(r.name)) {
          in <~ s" ${r.id.name}: ${r.name}\n"
        }
        in << ""
      }
  }

  def updateRealm(implicit ec: ExecutionContext, to: Timeout): Command = new Form {
    def fields = List("realm id", "realm name")
    def validateConvert = {
      case ("realm id", value) => Try(Ident(value))
    }
    def onComplete(in: Input) = {
      val realm = Realm(
        in.session[Ident]("realm id"),
        in.session[String]("realm name"))
      in << (for (_ <- in.porter ? UpdateRealm(realm)) yield {
        "Realm updated: "+ realm
      })
    }
    def show = {
      case in @ Input(msg, conn, porter, _) if msg.startsWith("update realm") =>
        conn ! tcp("Enter the realm properties.\n")
        true
    }
  }

  def deleteRealm(implicit ec: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, _, porter, _) if msg.startsWith("delete realm") =>
      in << (for {
        name <- Future.immediate(Ident(msg.substring("delete realm".length).trim))
        _ <- porter ? DeleteRealm(name)
      } yield "Realm deleted.")
  }
}

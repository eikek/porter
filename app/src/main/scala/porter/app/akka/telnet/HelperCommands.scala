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

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.model._
import scala.util.Try
import porter.app.akka.api.PorterMain.ShowSettings
import porter.app.akka.PorterUtil

object HelperCommands extends Commands {

  import akka.pattern.ask

  def makeDoc =
    """
      |Helper commands
      |---------------
      |crypt pw <plainpassw>    encrypt a given plaintext password which can be used
      |                         with ConfigStore or PropertyFileStore
      |show settings            shows the PorterSettings object that was used to create
      |                         this porter instance
      |add props                adds properties to a group or an account
      |remove props             removes properties from a group or an account
    """.stripMargin

  def make(implicit executor: ExecutionContext, to: Timeout) = Seq({
    case in@Input(help, conn, _, _) if help == "help" =>
      in << s"""
          |This is a simple interface for managing accounts.
          |
          |${TelnetConnection.documentation}
        """.stripMargin

    case in@Input(pwreq, conn, _, _) if pwreq.startsWith("crypt pw") =>
      val plain = pwreq.substring("crypt pw".length).trim
      if (plain.isEmpty)
        in << "Error: Empty password."
      else
        in << Password(PasswordCrypt.choose)(plain).asString

    case in@Input(show, conn, porter, _) if show == "show settings" =>
      val settings = (porter ? ShowSettings).mapTo[String]
      in << settings.map(s => s /* + s"\nPorter path: ${porter.porterPath}" */)

  }, manageProps("add"), manageProps("remove"))

  def manageProps(verb: String)(implicit ec: ExecutionContext, to: Timeout) = new Form {
    def fields = List("Group or Account (g/a)", "Name", "Properties")

    def show = {
      case in@Input(msg, conn, _, sess) if msg == (verb+" props") =>
        in.withRealm { _ =>
          conn ! tcp(s"Please enter account/group name and properties to $verb.\n")
        }
        sess.realm.isDefined
    }

    def validateConvert = {
      case ("Group or Account (g/a)", value) => Try {
        if (value.equalsIgnoreCase("g")) false
        else if (value.equalsIgnoreCase("a")) true
        else sys.error("Invalid value. Type 'a' or 'g'.")
      }
      case ("Name", value) => Try(Ident(value))
      case ("Properties", value) =>
        if (verb == "remove") Try(makeList(' ')(value))
        else makePairs(value)
    }

    def onComplete(in: Input) = {
      val isaccount = in.session[Boolean]("Group or Account (g/a)")
      val name = in.session[Ident]("Name")
      in.withRealm { realm =>
        if (isaccount) {
          val changeFun = if (verb == "remove") {
            val props = in.session[List[String]]("Properties")
            (a: Account) => a.updatedProps(p => p -- props)
          } else {
            val props = in.session[Map[String, String]]("Properties")
            (a: Account) => a.updatedProps(_ ++ props)
          }
          val f = PorterUtil.updateAccount(in.porter, realm.id, name, changeFun)
          in << f.map(of => if (of.success) "Success" else "Failed")
        } else {
          val changeFun = if (verb == "remove") {
            val props = in.session[List[String]]("Properties")
            (g: Group) => g.updatedProps(p => p -- props)
          } else {
            val props = in.session[Map[String, String]]("Properties")
            (g: Group) => g.updatedProps(_ ++ props)
          }
          val f = PorterUtil.updateGroup(in.porter, realm.id, name, changeFun)
          in << f.map(of => if (of.success) "Success" else "Failed")
        }
      }
    }
  }
}

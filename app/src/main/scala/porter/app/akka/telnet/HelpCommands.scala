package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.model.Secret

object HelpCommands extends Commands {

  def make(implicit executor: ExecutionContext, to: Timeout) = Seq({
    case in@Input(help, conn, _, _) if help == "help" =>
      in << """
          |This is a simple interface for managing accounts.
          |
          |TODO
        """.stripMargin

    case in@Input(pwreq, conn, _, _) if pwreq.startsWith("crypt pw") =>
      val plain = pwreq.substring("crypt pw".length).trim
      in << Secret.bcryptPassword(plain).asString

    case in@Input(show, conn, porter, _) if show == "show settings" =>
      val as = porter.settings.authenticators.mkString(" ", "\n", "")
      val ss = porter.settings.stores.mkString(" ", "\n", "")
      val ms = porter.settings.mutableStores.mkString(" ", "\n", "")
      in << s"""
          |Authenticators
          |--------------
          |$as
          |
          |Stores
          |------
          |$ss
          |
          |MutableStores
          |-------------
          |$ms
          |
          |Porter Actorpath: ${porter.porterPath}
        """.stripMargin
  })

}

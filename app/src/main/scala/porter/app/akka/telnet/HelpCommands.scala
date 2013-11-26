package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.model.Secret

object HelpCommands extends Commands {

  def make(implicit executor: ExecutionContext, to: Timeout) = Seq({
    case Input(help, conn, _, _) if help == "help" =>
      conn ! prompt(
        """
          |This is a simple interface for managing accounts.
          |
          |TODO
        """.stripMargin)

    case Input(pwreq, conn, _, _) if pwreq.startsWith("crypt pw") =>
      val plain = pwreq.substring("crypt pw".length).trim
      conn ! prompt(Secret.bcryptPassword(plain).asString)

    case Input(show, conn, porter, _) if show == "show settings" =>
      val as = porter.settings.authenticators.mkString(" ", "\n", "")
      val ss = porter.settings.stores.mkString(" ", "\n", "")
      val ms = porter.settings.mutableStores.mkString(" ", "\n", "")
      conn ! prompt(
        s"""
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
        """.stripMargin)
  })

}

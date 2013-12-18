package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.model.{Password, Secret}
import porter.app.akka.api.PorterMain.ShowSettings

object HelpCommands extends Commands {

  import akka.pattern.ask

  def makeDoc =
    """
      |Helper commands
      |---------------
      |crypt pw <plainpassw>    encrypt a given plaintext password which can be used
      |                         with ConfigStore or PropertyFileStore
      |show settings            shows the PorterSettings object that was used to create
      |                         this porter instance
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
        in << Password(plain).asString

    case in@Input(show, conn, porter, _) if show == "show settings" =>
      val settings = (porter ? ShowSettings()).mapTo[String]
      in << settings.map(s => s /* + s"\nPorter path: ${porter.porterPath}" */)

  })

}

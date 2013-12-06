package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.model.Secret
import porter.app.akka.api.PorterMain.ShowSettings

object HelpCommands extends Commands {

  import akka.pattern.ask

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
      val settings = (porter ? ShowSettings()).mapTo[String]
      in << settings.map(s => s /* + s"\nPorter path: ${porter.porterPath}" */)

  })

}

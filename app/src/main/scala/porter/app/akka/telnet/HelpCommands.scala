package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.model.Secret

/**
 *
 * @since 25.11.13 09:54
 *
 */
object HelpCommands extends Commands {

  def make(implicit executor: ExecutionContext, to: Timeout) = Seq({
    case Input(help, conn, _, _) if help == "help" =>
      conn ! tcp(
        """
          |This is a simple interface for managing accounts.
          |
          |TODO
        """.stripMargin)

    case Input(pwreq, conn, _, _) if pwreq.startsWith("crypt pw") =>
      val plain = pwreq.substring("crypt pw".length).trim
      conn ! tcp(Secret.bcryptPassword(plain).asString)
  })

}

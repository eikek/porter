package porter.app.akka.telnet

import akka.actor.{Props, Terminated, ActorRef, Actor}
import akka.io.Tcp
import akka.util.{Timeout, ByteString}
import porter.BuildInfo
import scala.concurrent.ExecutionContext

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 23:16
 */
private[telnet] class TelnetConnection(porter: ActorRef, conn: ActorRef) extends Actor {
  context.watch(conn)

  implicit val executor = context.dispatcher
  implicit val timeout = Timeout(5000)

  private val ctrl_d = 4.toByte
  private val quit_requested = "quit requested"
  private val session = new Session(None)
  private val cmds = TelnetConnection.allCommands.reduce

  def receive = {
    case Tcp.Received(data) if data.utf8String.trim == "\\q" =>
      session.add(quit_requested, true)
      conn ! tcp("This will shutdown porter with all running services (telnet, http).\nReally? (yes/no): ")
    case Tcp.Received(data) if session.get(quit_requested) == Some(true) =>
      if (data.utf8String.trim equalsIgnoreCase "yes") {
        context.system.shutdown()
      } else {
        session.remove(quit_requested)
        conn ! prompt("")
      }

    case Tcp.Received(data) if data.utf8String.trim == "exit" || data.head == ctrl_d =>
      conn ! Tcp.Write(ByteString("Good bye.\n"))
      context.stop(self)

    case Tcp.Received(data) =>
      cmds(Input(data.utf8String.trim, conn, porter, session))

    case x: Tcp.ConnectionClosed =>
      context.stop(self)

    case Terminated(`conn`) =>
      context.stop(self)

    case Terminated(`porter`) =>
      conn ! Tcp.Write(ByteString("Porter actor terminated. Sorry, closing connection."))
      context.stop(self)
  }

  override def preStart() = {
    val welcome = prompt(
      s"""
        |Welcome to porter ${BuildInfo.version}
        |
        |Type 'help' for a list of available commands.
        |""".stripMargin)
    conn ! welcome
  }
}

object TelnetConnection {
  private[telnet] def props(porter:ActorRef, conn: ActorRef) = Props(classOf[TelnetConnection], porter, conn)

  private val allCommands = HelpCommands ++ RealmCommands ++ AccountCommands ++ GroupCommands ++ AuthCommands
  val documentation = allCommands.makeDoc
}

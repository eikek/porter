package porter.app.akka.telnet

import akka.actor.{Terminated, ActorRef, Actor}
import porter.app.akka.PorterExt
import akka.io.Tcp
import akka.util.{Timeout, ByteString}
import porter.BuildInfo

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 23:16
 */
class TelnetConnection(conn: ActorRef, server: ActorRef) extends Actor {
  val porter = PorterExt(context.system)
  context.watch(conn)
  context.watch(porter.ref)

  implicit val executor = context.dispatcher
  implicit val timeout = Timeout(5000)

  private val session = new Session(None)
  private val cmds = (HelpCommands ++ RealmCommands ++ AccountCommands).reduce

  def receive = {
    case Tcp.Received(data) if data.utf8String.trim == "\\q" =>
      context.system.shutdown()

    case Tcp.Received(data) if data.utf8String.trim == "exit" =>
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
        |""".stripMargin)
    conn ! welcome
  }
}

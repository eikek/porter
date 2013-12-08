package porter.dist

import _root_.akka.io.Tcp
import _root_.akka.util.Timeout
import porter.app.akka.telnet.TelnetServer
import porter.app.akka.http.HttpHandler
import spray.can.Http
import porter.app.PorterSettings
import porter.app.akka.api.PorterMain
import akka.actor.ActorSystem
import scala.concurrent.Future
import akka.io.Tcp.CommandFailed
import porter.app.akka.Porter

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 14:40
 */
object Main extends App {

  // starts akka system
  implicit val system = ActorSystem("porter")
  import system.dispatcher

  // start porter actor and two interfaces: http and telnet
  import scala.concurrent.duration._
  implicit val bindTimeout = Timeout(2.seconds)

  val main = MainExt(system)
  val porter = Porter(system).main
  println(s"\n---\n--- Porter remote actor listening on ${main.pathFor(porter)} \n---")

  val telnetF = if (main.telnet.enabled) {
    TelnetServer.bind(porter, main.telnet.host, main.telnet.port)
  } else Future.failed(new Exception("telnet not active"))

  val httpF = if (main.http.enabled) {
    HttpHandler.bind(porter, main.http.host, main.http.port)
  } else Future.failed(new Exception("http not active"))

  for (telnetb <- telnetF; httpb <- httpF) {
    httpb match {
      case Http.Bound(a) => println("--- Bound http to " + a)
      case CommandFailed(c) => println("---x Failed to bind http")
    }
    telnetb match {
      case Tcp.Bound(a) => println("--- Bound telnet to " + a)
      case CommandFailed(c) => println("---x Failed to bind telnet")
    }
    println("---")
  }

}

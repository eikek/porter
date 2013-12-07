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

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 14:40
 */
object Main extends App {

  // there are only two options: --help or -h, or starting porter
  if (args.contains("-h") || args.contains("--help") || args.length > 0) {
    val bi = porter.BuildInfo
    println(s"Porter ${bi.version} (rev ${bi.revision} build on ${new java.util.Date(bi.buildTime)})")
    println()
    println(
      """
        |Options are:
        |   --help or -h:     print this message
        |   <nothing>         start porter. please use system property
        |                     config.file to specify a new configuration
        |                     file. The file will replace the default one,
        |                     so to overwrite tings, just include it at
        |                     the beginning with `include "application"`.
        |                     see https://github.com/typesafehub/config
      """.stripMargin)

  } else {
    // starts akka system
    implicit val system = ActorSystem("porter")
    import system.dispatcher

    // start porter actor and two interfaces: http and telnet
    import scala.concurrent.duration._
    implicit val bindTimeout = Timeout(2.seconds)

    val config = system.settings.config.getConfig("porter")
    val settings = PorterSettings.fromConfig(config)
    val porter = system.actorOf(PorterMain.props(settings), name = "porter-api")
    val path = Porter(system).porterPath(porter)
    println(s"\n---\n--- Porter remote actor listening on $path \n---")

    val telnetF = if (config.getBoolean("telnet.enabled")) {
      TelnetServer.bind(porter, config.getString("telnet.host"), config.getInt("telnet.port"))
    } else Future.failed(new Exception("telnet not active"))

    val httpF = if (config.getBoolean("http.enabled")) {
      HttpHandler.bind(porter, config.getString("http.host"), config.getInt("http.port"))
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

}

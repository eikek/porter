package porter.app

import _root_.akka.actor.{Props, ActorSystem}
import _root_.akka.io.{IO, Tcp}
import _root_.akka.util.Timeout
import com.typesafe.config.ConfigFactory
import porter.app.akka.PorterExt
import java.net.InetSocketAddress
import porter.app.akka.telnet.TelnetServer

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 14:40
 */
object PorterMain extends App {

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
    import _root_.akka.pattern.ask
    implicit val bindTimeout = Timeout(2.seconds)

    val telnetEndpoint = new InetSocketAddress("localhost", 2345)
    val telnetServer = system.actorOf(Props(classOf[TelnetServer]), name = "porter-telnet")

    val telnetF = IO(Tcp) ? Tcp.Bind(telnetServer, telnetEndpoint)
    telnetF.onSuccess { case Tcp.Bound(addr) =>
      println("Bound telnet to " + addr)
    }

    // print what's going on
  }

}

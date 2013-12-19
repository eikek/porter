package porter.dist

import akka.actor._
import scala.util.control.NonFatal

/**
 * Starts the [[porter.dist.MainActor]]
 */
object Main {

  val name = "porter"

  def main(args: Array[String]): Unit = {
    if (args.length != 0) {
      println("This app takes no arguments. They are ignored.")
    }
    val system = ActorSystem(name)
    try {
      val app = system.actorOf(MainActor(), name)
      system.actorOf(Props(classOf[Terminator], app), name+"-terminator")
    } catch {
      case NonFatal(e) ⇒ system.shutdown(); throw e
    }
  }

  class Terminator(app: ActorRef) extends Actor with ActorLogging {
    context watch app
    def receive = {
      case Terminated(_) ⇒
        log.info("application supervisor has terminated, shutting down")
        context.system.shutdown()
    }
  }

}

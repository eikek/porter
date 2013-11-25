package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import porter.app.akka.PorterActor.FindRealm
import porter.model.Realm
import scala.util.{Failure, Success}
import akka.util.Timeout
import porter.app.akka.PorterActor

/**
 *
 * @since 25.11.13 09:39
 *
 */
object RealmCommands extends Commands {

  import akka.pattern.ask

  def make(implicit executor: ExecutionContext, to: Timeout): Seq[Command] =
    List(changeRealm, listRealms)


  def changeRealm(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, sess) if msg.startsWith("use realm") =>
      val realmname = msg.substring("use realm".length).trim
      val f = (porter ? FindRealm(Set(realmname))).mapTo[Iterable[Realm]]
      in.andThen(f) { iter =>
        sess.realm = iter.headOption
        if (iter.headOption.isDefined) conn ! tcp("Using realm "+ iter)
        else conn ! tcp("Realm not found")
      }
  }

  def listRealms(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg == "lr" =>
      val f = (porter ? PorterActor.ListRealms).mapTo[Iterable[Realm]]
      in.andThen(f) {
        case iter =>
          val s = new StringBuilder(s"Realm list (${iter.size})\n")
          s append iter.map(r => s"${r.id}: ${r.name}").mkString(" ", "\n", "")
          conn ! tcp(s.toString())
      }
  }
}

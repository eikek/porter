package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import porter.app.akka.PorterActor.{DeleteRealm, FindRealm}
import porter.model.{Ident, Realm}
import scala.util.{Failure, Success}
import akka.util.Timeout
import porter.app.akka.PorterActor

object RealmCommands extends Commands {

  import akka.pattern.ask

  def make(implicit executor: ExecutionContext, to: Timeout): Seq[Command] =
    List(changeRealm, listRealms, updateRealm, deleteRealm)


  def changeRealm(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, sess) if msg.startsWith("use realm") =>
      val realmname = msg.substring("use realm".length).trim
      val f = (porter.ref ? FindRealm(Set(realmname))).mapTo[Iterable[Realm]]
      in.onSuccess(f) { iter =>
        sess.realm = iter.headOption
        if (iter.headOption.isDefined) conn ! prompt("Using realm "+ iter)
        else conn ! prompt("Realm not found")
      }
  }

  def listRealms(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg == "lr" =>
      val f = (porter.ref ? PorterActor.ListRealms).mapTo[Iterable[Realm]]
      in.onSuccess(f) {
        case iter =>
          val s = new StringBuilder(s"Realm list (${iter.size})\n")
          s append iter.map(r => s"${r.id}: ${r.name}").mkString(" ", "\n", "")
          conn ! prompt(s.toString())
      }
  }

  def updateRealm(implicit ec: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg.startsWith("update realm") =>
      val pairs = Commands.makePairs(msg.substring("update realm".length).trim)
      in.onSuccess(pairs) { p =>
        val id = p.get("id").map(Ident.apply).getOrElse(Ident.randomIdent)
        val name = p.get("name").getOrElse("")
        val realm = Realm(id, name)
        val f = (porter.ref ? PorterActor.UpdateRealm(realm)).mapTo[Boolean]
        in.onSuccess(f) { b =>
          conn ! (if (b) prompt("Realm created.") else prompt("Could not create realm."))
        }
      }
  }

  def deleteRealm(implicit ec: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg.startsWith("delete realm") =>
      val name = msg.substring("delete realm".length).trim
      in.onSuccess(porter.ref ? DeleteRealm(name)) { x =>
        conn ! prompt("Realm deleted.")
      }
  }
}

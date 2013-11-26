package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import porter.app.akka.PorterActor.{UpdateRealm, DeleteRealm, FindRealm}
import porter.model.{Ident, Realm}
import scala.util.{Try, Failure, Success}
import akka.util.Timeout
import porter.app.akka.PorterActor

object RealmCommands extends Commands {

  import akka.pattern.ask

  def make(implicit executor: ExecutionContext, to: Timeout): Seq[Command] =
    List(changeRealm, listRealms, updateRealm, deleteRealm)


  def changeRealm(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, sess) if msg.startsWith("use realm") =>
      val realmname = Try(Ident(msg.substring("use realm".length).trim))
      in.onSuccess(realmname) { rid =>
        val f = (porter.ref ? FindRealm(Set(rid))).mapTo[Iterable[Realm]]
        in.onSuccess(f) { iter =>
          sess.realm = iter.headOption
          if (iter.headOption.isDefined) in << s"Using realm ${sess.realm.get.id.name}: ${sess.realm.get.name}"
          else in << "Realm not found"
        }
      }
  }

  def listRealms(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg == "lr" =>
      val f = (porter.ref ? PorterActor.ListRealms).mapTo[Iterable[Realm]]
      in.onSuccess(f) {
        case iter =>
          val s = new StringBuilder(s"Realm list (${iter.size})\n")
          s append iter.map(r => s"${r.id}: ${r.name}").mkString(" ", "\n", "")
          in << s.toString()
      }
  }

  def updateRealm(implicit ec: ExecutionContext, to: Timeout): Command = new Form {
    def validateConvert = {
      case ("realm id", value) => Try(Ident(value))
    }

    def onComplete(in: Input) = {
      val realm = Realm(
        in.session[Ident]("realm id"),
        in.session[String]("realm name"))
      in.onSuccess(in.porter.ref ? UpdateRealm(realm)) { _ =>
        in << ("Realm updated: "+ realm)
      }
    }

    def fields = List("realm id", "realm name")

    def show = {
      case in @ Input(msg, conn, porter, _) if msg.startsWith("update realm") =>
        conn ! tcp("Enter the realm properties.\n")
        true
    }
  }

  def deleteRealm(implicit ec: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg.startsWith("delete realm") =>
      val name = msg.substring("delete realm".length).trim
      in.onSuccess(porter.ref ? DeleteRealm(name)) { x =>
        in << "Realm deleted."
      }
  }
}

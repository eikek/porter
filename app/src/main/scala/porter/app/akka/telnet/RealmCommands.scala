package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import porter.app.akka.PorterActor.{UpdateRealm, DeleteRealm, FindRealm}
import porter.model.{Ident, Realm}
import scala.util.{Try, Failure, Success}
import akka.util.Timeout
import porter.app.akka.PorterActor

object RealmCommands extends Commands {

  import akka.pattern.ask
  import porter.util._

  def make(implicit executor: ExecutionContext, to: Timeout): Seq[Command] =
    List(changeRealm, listRealms, updateRealm, deleteRealm)

  def changeRealm(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, sess) if msg.startsWith("use realm") =>
      in << (for {
        realm <- Future.immediate(Ident(msg.substring("use realm".length).trim))
        iter <- (porter.ref ? FindRealm(Set(realm))).mapTo[Iterable[Realm]]
      } yield {
        sess.realm = iter.headOption
        if (iter.headOption.isDefined) s"Using realm ${sess.realm.get.id.name}: ${sess.realm.get.name}"
        else "Realm not found"
      })
  }

  def listRealms(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg == "lr" =>
      in << (for (iter <- (porter.ref ? PorterActor.ListRealms).mapTo[Iterable[Realm]]) yield {
        s"Realm list (${iter.size})\n" + iter.map(r => s"${r.id}: ${r.name}").mkString(" ", "\n", "")
      })
  }

  def updateRealm(implicit ec: ExecutionContext, to: Timeout): Command = new Form {
    def fields = List("realm id", "realm name")
    def validateConvert = {
      case ("realm id", value) => Try(Ident(value))
    }
    def onComplete(in: Input) = {
      val realm = Realm(
        in.session[Ident]("realm id"),
        in.session[String]("realm name"))
      in << (for (_ <- in.porter.ref ? UpdateRealm(realm)) yield {
        "Realm updated: "+ realm
      })
    }
    def show = {
      case in @ Input(msg, conn, porter, _) if msg.startsWith("update realm") =>
        conn ! tcp("Enter the realm properties.\n")
        true
    }
  }

  def deleteRealm(implicit ec: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, _, porter, _) if msg.startsWith("delete realm") =>
      in << (for {
        name <- Future.immediate(Ident(msg.substring("delete realm".length).trim))
        _ <- porter.ref ? DeleteRealm(name)
      } yield "Realm deleted.")
  }
}

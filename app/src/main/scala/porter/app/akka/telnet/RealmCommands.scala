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
      for {
        realm <- in.realmFuture
        iter <- (porter.ref ? FindRealm(Set(realm.id))).mapTo[Iterable[Realm]]
      } {
        sess.realm = iter.headOption
        if (iter.headOption.isDefined) in << s"Using realm ${sess.realm.get.id.name}: ${sess.realm.get.name}"
        else in << "Realm not found"
      }
  }

  def listRealms(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, _) if msg == "lr" =>
      for (iter <- (porter.ref ? PorterActor.ListRealms).mapTo[Iterable[Realm]]) {
        val s = new StringBuilder(s"Realm list (${iter.size})\n")
        s append iter.map(r => s"${r.id}: ${r.name}").mkString(" ", "\n", "")
        in << s.toString()
      }
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
      for (_ <- in.porter.ref ? UpdateRealm(realm)) {
        in << ("Realm updated: "+ realm)
      }
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

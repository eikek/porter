package porter.app.akka.telnet

import scala.concurrent.{Future, Promise, ExecutionContext}
import akka.util.Timeout
import porter.model._
import scala.util.Try
import porter.model.Group
import porter.app.akka.PorterActor.FindGroup
import porter.app.akka.PorterActor.MakeRules
import porter.app.akka.PorterActor.GroupResponse
import porter.app.akka.PorterActor.DeleteGroup
import porter.app.akka.PorterActor.UpdateGroup
import porter.app.akka.PorterActor.MakeRulesResponse
import porter.app.akka.PorterActor.ListGroups

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 00:27
 */
object GroupCommands extends Commands {
  import akka.pattern.ask
  import porter.util._

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(listall, update, delete, addRules, removeRules)

  def listall(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg == "lg" =>
      val result = for {
        r <- in.realmFuture
        groups <- (porter.ref ? ListGroups(r.id)).mapTo[Iterable[Group]]
      } yield {
        groups map { g =>
          val gname = g.name.name
          val rules = g.rules.mkString("\n  ", "\n  ", "\n")
          val props = g.props.map(p => p._1+"="+p._2).mkString("[", ",","]")
          s"$gname: $rules $props"
        }
      }
      in <<< result
  }

  def update(implicit executor: ExecutionContext, to: Timeout): Command = new Form {
    def fields = List("name", "rules", "props")

    def show = {
      case in@Input(msg, conn, _, _) if msg == "update group" =>
        in.withRealm { realm =>
          conn ! tcp("Enter group details.\n")
        }
        in.session.realm.isDefined
    }

    def validateConvert = {
      case ("name", value) => Try(Ident(value))
      case ("rules", value) => Try(Commands.makeList(',')(value).map(_.trim).toSet)
      case ("props", value) => Commands.makePairs(value)
    }

    def onComplete(in: Input) = {
      val g = Group(
        in.session[Ident]("name"),
        in.session[Properties]("props"),
        in.session[Set[String]]("rules")
      )
      val result = for {
        r <- in.realmFuture
        _ <- in.porter.ref ? UpdateGroup(r.id, g)
      } yield "Group updated."
      in << result
    }
  }

  def delete(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("delete group") =>
      val f = for {
        r <- in.realmFuture
        gid <- Future.immediate(Ident(msg.substring("delete group".length).trim))
        _ <- porter.ref ? DeleteGroup(r.id, gid)
      } yield "Group deleted."
      in << f
  }

  def addRules(implicit executor: ExecutionContext, to: Timeout) = new Form {
    def fields = List("group", "rules")

    def show = {
      case in@Input(msg, _, _, _) if msg == "add rules" =>
        in.withRealm { r =>
          in.conn ! tcp("Enter group name and list of rules (separated by space) to add.\n")
        }
        in.session.realm.isDefined
    }

    def validateConvert = {
      case ("group", value) => Try(Ident(value))
      case ("rules", value) => Try(Commands.makeList(' ')(value).map(_.trim).toSet)
    }

    def onComplete(in: Input) = {
      val group = in.session[Ident]("group")
      val perms = in.session[Set[String]]("rules")
      val rules = for {
        r <- in.realmFuture
        resp <- (in.porter.ref ? FindGroup(r.id, Set(group))).mapTo[GroupResponse]
        g <- Future.immediate(resp.groups.toList.headOption, "Group not found")
        current <- (in.porter.ref ? MakeRules(g.rules)).mapTo[MakeRulesResponse]
        remove <- (in.porter.ref ? MakeRules(perms)).mapTo[MakeRulesResponse]
      } yield {
        val perms = Permission.union(current.permissions, remove.permissions)
        val revocs = Revocation.union(current.revocations, remove.revocations)
        (g, perms.map(_.toString) ++ revocs.map(_.toString))
      }
      val result = for {
        r <- in.realmFuture
        (g, next) <- rules
        _ <- in.porter.ref ? UpdateGroup(r.id, g.copy(rules = next))
      } yield "Rules added."
      in << result
    }
  }

  def removeRules(implicit executor: ExecutionContext, to: Timeout) = new Form {
    def fields = List("group", "rules")
    def show = {
      case in@Input(msg, _, _, _) if msg == "remove rules" =>
        in.withRealm { r =>
          in.conn ! tcp("Enter group name and list of rules (separated by space) to remove.\n" +
            "Note that all rules are removed that are implied by the given ones!\n")
        }
        in.session.realm.isDefined
    }
    def validateConvert = {
      case ("group", value) => Try(Ident(value))
      case ("rules", value) => Try(Commands.makeList(' ')(value).map(_.trim).toSet)
    }
    def onComplete(in: Input) {
      val group = in.session[Ident]("group")
      val perms = in.session[Set[String]]("rules")
      val rules = for {
        r <- in.realmFuture
        resp <- (in.porter.ref ? FindGroup(r.id, Set(group))).mapTo[GroupResponse]
        g <- Future.immediate(resp.groups.toList.headOption, "Group not found")
        current <- (in.porter.ref ? MakeRules(g.rules)).mapTo[MakeRulesResponse]
        remove <- (in.porter.ref ? MakeRules(perms)).mapTo[MakeRulesResponse]
      } yield {
        val perms = Permission.diff(current.permissions, remove.permissions)
        val revocs = Revocation.diff(current.revocations, remove.revocations)
        (g, perms.map(_.toString) ++ revocs.map(_.toString))
      }
      val result = for {
        r <- in.realmFuture
        (g, next) <- rules
        _ <- in.porter.ref ? UpdateGroup(r.id, g.copy(rules = next))
      } yield "Rules removed."
      in << result
    }
  }
}
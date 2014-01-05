package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import porter.model._
import scala.util.Try
import porter.model.Group
import porter.app.akka.Porter

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 00:27
 */
object GroupCommands extends Commands {
  import akka.pattern.ask
  import Porter.Messages.store._
  import Porter.Messages.mutableStore._
  import Porter.Messages.rules._
  import porter.util._

  def makeDoc =
    """
      |Group commands
      |--------------
      |lg                  list all groups
      |update group        update an existing group or adds a new one
      |delete group <name> deletes a group with the given name
      |add rules           add rules to an existing group
      |remove rules        remove rules from a group
    """.stripMargin

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(listall, update, delete, manageRules("add",
      Permission.union, Revocation.union), manageRules("remove", Permission.diff, Revocation.diff))

  def listall(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg == "lg" =>
      val result = for {
        r <- in.realmFuture
        groups <- (porter ? GetAllGroups(r.id)).mapTo[FindGroupsResp].map(_.groups)
      } yield {
        groups map { g =>
          val gname = g.name.name
          val rules = g.rules.mkString("\n  ", "\n  ", "\n")
          val props = g.props.map(p => p._1+"="+p._2).mkString("  |", "\n  |", "")
          s"$gname: $rules$props"
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
      case ("rules", value) => Try(makeList(' ')(value).map(_.trim).toSet)
      case ("props", value) => makePairs(value)
    }

    def onComplete(in: Input) = {
      val g = Group(
        in.session[Ident]("name"),
        in.session[Properties]("props"),
        in.session[Set[String]]("rules")
      )
      val result = for {
        r <- in.realmFuture
        _ <- in.porter ? UpdateGroup(r.id, g)
      } yield "Group updated."
      in << result
    }
  }

  def delete(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("delete group") =>
      val f = for {
        r <- in.realmFuture
        gid <- Future.immediate(Ident(msg.substring("delete group".length).trim))
        _ <- porter ? DeleteGroup(r.id, gid)
      } yield "Group deleted."
      in << f
  }

  def manageRules(verb: String,
                  pf: (Set[Permission], Set[Permission]) => Set[Permission],
                  rf: (Set[Revocation], Set[Revocation]) => Set[Revocation])
                 (implicit executor: ExecutionContext, to: Timeout) = new Form {
    def fields = List("group", "rules")
    def show = {
      case in@Input(msg, _, _, _) if msg == (verb +" rules") =>
        in.withRealm { r =>
          in.conn ! tcp(s"Enter group name and list of rules (separated by space) to $verb.\n" +
            s"Note that this applies to all rules implied by the given ones!\n")
        }
        in.session.realm.isDefined
    }
    def validateConvert = {
      case ("group", value) => Try(Ident(value))
      case ("rules", value) => Try(makeList(' ')(value).map(_.trim).toSet)
    }
    def onComplete(in: Input) = {
      val group = in.session[Ident]("group")
      val perms = in.session[Set[String]]("rules")
      val rules = for {
        r <- in.realmFuture
        resp <- (in.porter ? FindGroups(r.id, Set(group))).mapTo[FindGroupsResp]
        g <- Future.immediate(resp.groups.toList.headOption, "Group not found")
        current <- (in.porter ? MakeRules(g.rules)).mapTo[MakeRulesResp]
        remove <- (in.porter ? MakeRules(perms)).mapTo[MakeRulesResp]
      } yield {
        val perms = pf(current.permissions, remove.permissions)
        val revocs = rf(current.revocations, remove.revocations)
        (g, perms.map(_.toString) ++ revocs.map(_.toString))
      }
      val result = for {
        r <- in.realmFuture
        (g, next) <- rules
        _ <- in.porter ? UpdateGroup(r.id, g.copy(rules = next))
      } yield "Successful."
      in << result
    }
  }
}
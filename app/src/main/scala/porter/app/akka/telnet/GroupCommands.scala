package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.app.akka.PorterActor.{DeleteGroup, UpdateGroup, ListGroups}
import porter.model.{Properties, Ident, Group}
import scala.util.Try

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 27.11.13 00:27
 */
object GroupCommands extends Commands {
  import akka.pattern.ask

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(listall, update, delete)

  def listall(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg == "lg" =>
      in.withRealm { realm =>
        in.onSuccess((porter.ref ? ListGroups(realm.id)).mapTo[Iterable[Group]]) { groups =>
          val gs = groups.toList.sortBy(_.name.name).map(g =>
            s"${g.name.name}: ${g.rules.mkString("\n  ", "\n  ", "\n")}  ${g.props.map(p => p._1+"="+p._2).mkString("[", ",","]")}")
          in << gs.mkString("\n")
        }
      }
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
      in.withRealm { realm =>
        in.onSuccess(in.porter.ref ? UpdateGroup(realm.id, g)) { _=>
          in << "Group updated."
        }
      }
    }
  }

  def delete(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("delete group") =>
      in.withRealm { realm =>
        val name = Try(Ident(msg.substring("delete group".length).trim))
        in.onSuccess(name) { gid =>
          in.onSuccess(porter.ref ? DeleteGroup(realm.id, gid)) { _=>
            in << "Group deleted"
          }
        }
      }
  }
}

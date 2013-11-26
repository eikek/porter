package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import scala.util.Try
import porter.model.{Group, Ident, PasswordCredentials}
import porter.app.akka.PorterActor._
import porter.app.akka.PorterActor.AuthzResponse
import porter.app.akka.PorterActor.AuthResponse
import scala.util.Failure
import porter.app.akka.PorterActor.Authenticate
import porter.app.akka.PorterActor.Authorized

object AuthCommands extends Commands {
  import akka.pattern.ask

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(authenticate, authorize, showPolicy)


  def authenticate(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("authc") =>
      in.withRealm { realm =>
        val creds = msg.substring("authc".length).trim.split("\\s+").toList match {
          case a::b::Nil => Try(PasswordCredentials(a.trim, b.trim))
          case _ => Failure(new IllegalArgumentException(s"Cannot get username-password pair from: '$msg'"))
        }
        in.onSuccess(creds) { up =>
          in.onSuccess((porter.ref ? Authenticate(realm.id, Set(up))).mapTo[AuthResponse]) { res =>
            val s = res.token.votes.map({ case(n, v) => s"${n.name} -> $v" })
            in << s.mkString("\n")
          }
        }
      }
  }

  def authorize(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("authz") =>
      in.withRealm { realm =>
        val perms = msg.substring("authc".length).trim.split("\\s+", 2).toList match {
          case a::b::Nil => Try(Ident(a.trim) -> Commands.makeList(' ')(b.trim).map(_.trim).toSet)
          case _ => Failure(new IllegalArgumentException(s"Cannot get username-password pair from: '$msg'"))
        }
        in.onSuccess(perms) { case (login, plist) =>
          in.onSuccess((porter.ref ? Authorized(realm.id, login, plist)).mapTo[AuthzResponse]) { resp =>
            in << s"Result: ${resp.result}"
          }
        }
      }
  }

  def showPolicy(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("show policy") =>
      in.withRealm { realm =>
        val loginTry = Try(Ident(msg.substring("show policy".length).trim))
        in.onSuccess(loginTry) { login =>
          val fp = (porter.ref ? GetPolicy(realm.id, login)).mapTo[PolicyResponse]
          val fg = (porter.ref ? ListGroups(realm.id)).mapTo[Iterable[Group]]
          val fstring = for (pr <- fp; gr <- fg) yield {
            for {
              rule <- pr.policy.rules.toList.sortBy(_.isLeft)
            } yield {
              val perms = rule.fold(_.toString, _.toString)
              val groups = gr.filter(g => g.rules.contains(perms)).map(g => g.name.name)
              s"$perms ${groups.mkString("(", ",", ")")}"
            }
          }
          in.onSuccess(fstring) { outs =>
            in << outs.mkString("\n")
          }
        }
      }
  }
}

package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import scala.util.Try
import porter.model.{Ident, PasswordCredentials}
import scala.util.Failure
import porter.app.akka.Porter

object AuthCommands extends Commands {
  import akka.pattern.ask
  import porter.util._
  import Porter.Messages.authc._
  import Porter.Messages.authz._
  import Porter.Messages.store._

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(authenticate, authorize, showPolicy)


  def authenticate(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("authc") =>
      val creds = msg.substring("authc".length).trim.split("\\s+").toList match {
        case a::b::Nil => Try(PasswordCredentials(a.trim, b.trim))
        case _ => Failure(new IllegalArgumentException(s"Cannot get username-password pair from: '$msg'"))
      }
      val result = for {
        realm <- in.realmFuture
        up <- Future.immediate(creds)
        res <- (porter ? Authenticate(realm.id, Set(up))).mapTo[AuthenticateResp]
      } yield res.result.map(_.votes.map({ case(n, v) => s"${n.name} -> $v" })).getOrElse(List("Invalid credentials"))
      in <<< result
  }

  def authorize(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("authz") =>
      val perms = msg.substring("authz".length).trim.split("\\s+", 2).toList match {
        case a::b::Nil => Try(Ident(a.trim) -> Commands.makeList(' ')(b.trim).map(_.trim).toSet)
        case _ => Failure(new IllegalArgumentException(s"Cannot get permissions from: '$msg'"))
      }
      val result = for {
        realm <- in.realmFuture
        (login, plist) <- Future.immediate(perms)
        resp <- (porter ? Authorize(realm.id, login, plist)).mapTo[AuthorizeResp]
      } yield s"Result: ${resp.authorized}"
      in << result
  }

  def showPolicy(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("show policy") =>
      val result = for {
        r <- in.realmFuture
        login <- Future.immediate(Ident(msg.substring("show policy".length).trim))
        fp <- (porter ? GetPolicy(r.id, login)).mapTo[GetPolicyResp]
        fg <- (porter ? GetAllGroups(r.id)).mapTo[FindGroupsResp].map(_.groups)
      } yield {
        for (rule <- fp.policy.rules.toList.sortBy(_.isLeft)) yield {
          val perms = rule.fold(_.toString, _.toString)
          val groups = fg.filter(g => g.rules.contains(perms)).map(g => g.name.name)
          s"$perms ${groups.mkString("(", ",", ")")}"
        }
      }
      in <<< result
  }
}

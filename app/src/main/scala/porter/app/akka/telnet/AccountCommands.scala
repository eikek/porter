package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import porter.app.akka.PorterActor._
import porter.model.{Properties, Secret, Ident}
import scala.util.Try
import porter.app.akka.PorterActor.DeleteAccount
import porter.app.akka.PorterActor.UpdateAccount
import porter.app.akka.PorterActor.FindAccount
import porter.model.Account
import porter.app.akka.PorterActor.ListAccounts

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 26.11.13 00:06
 */
object AccountCommands extends Commands {
  import akka.pattern.ask
  import porter.util._

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(update, listall, delete, changePassword)

  def update(implicit executor: ExecutionContext, to: Timeout): Command = new Form {
    def fields = AccountDetails.all

    def validateConvert = {
      case (key, value) if key == AccountDetails.name =>
        Try(Ident(value))
      case (key, value) if key == AccountDetails.groups =>
        Try { Commands.makeList(',')(value).map(s => Ident(s.trim)).toSet }
      case (key, value) if key == AccountDetails.props =>
        Commands.makePairs(value)
    }

    def show = {
      case in @ Input(msg, conn, porter, sess) if msg == "update account" =>
        in.withRealm { r =>
          conn ! tcp("Enter the account details.\n")
        }
        sess.realm.isDefined
    }

    def onComplete(in: Input) = {
      val ad = AccountDetails.toAccount(in.session)
      val result = for {
        r <- in.realmFuture
        _ <- in.porter.ref ? UpdateAccount(r.id, ad.get)
      } yield "Account updated."
      in << result
    }
  }

  def listall(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @Input(msg, conn, porter, _) if msg == "la" =>
      val result = for {
        r <- in.realmFuture
        list <- (porter.ref ? ListAccounts(r.id)).mapTo[Iterable[Account]]
      } yield {
        val groups = (a: Account) => a.groups.map(_.name).mkString("(", ",", ")")
        val props = (a: Account) => a.props.map({case (k,v) => k +"="+v}).mkString("[", ",", "]")
        list.map(a => s"${a.name.name} ${groups(a)} ${props(a)}")
      }
      in <<< result
  }

  def delete(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("delete account") =>
      val result = for {
        realm <- in.realmFuture
        id <- Future.immediate(Ident(msg.substring("delete account".length).trim))
        _ <- porter.ref ? DeleteAccount(realm.id, id)
      } yield "Account deleted."
      in << result
  }

  def changePassword(implicit ec: ExecutionContext, to: Timeout): Command = new Form {
    def fields = List("login", "password")

    def show = {
      case in@Input(msg, conn, _, sess) if msg == "change pass" =>
        in.withRealm { r =>
          conn ! tcp("Enter the login name and a new password.\n")
        }
        sess.realm.isDefined
    }

    def validateConvert = {
      case (key, value) if key == "login" => Try(Ident(value))
    }

    def onComplete(in: Input) = {
      val login = in.session[Ident]("login")
      val passw = in.session[String]("password")
      val result = for {
        r <- in.realmFuture
        resp <- (in.porter.ref ? FindAccount(r.id, Set(login))).mapTo[AccountResponse]
        first <- Future.immediate(resp.accounts.headOption, "Account not found")
        _ <- in.porter.ref ? UpdateAccount(r.id, first.changeSecret(Secret.bcryptPassword(passw)))
      } yield "Password changed."
      in << result
    }
  }

  object AccountDetails {
    val name = "name"
    val groups = "groups"
    val password = "password"
    val props = "props"
    val all = List(name, groups, password, props)

    def toAccount(sess: Session) = Try(Account(
      sess[Ident](name),
      sess[Properties](props),
      sess[Set[Ident]](groups),
      Seq(Secret.bcryptPassword(sess[String](password)))
    ))
  }
}

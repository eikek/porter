package porter.app.akka.telnet

import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import porter.model.{Properties, Secret, Ident}
import scala.util.Try
import porter.model.Account
import porter.app.akka.Porter

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 26.11.13 00:06
 */
object AccountCommands extends Commands {
  import akka.pattern.ask
  import porter.util._
  import Porter.Messages.mutableStore._
  import Porter.Messages.store._


  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(update, listall, delete, changePassword,
      manageGroups("add", (a, b) => a++b), manageGroups("remove", (a, b) => a -- b))

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
        op <- (in.porter ? UpdateAccount(r.id, ad.get)).mapTo[OperationFinished]
      } yield "Account updated: " + op.result
      in << result
    }
  }

  def listall(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @Input(msg, conn, porter, _) if msg == "la" =>
      val result = for {
        r <- in.realmFuture
        list <- (porter ? GetAllAccounts(r.id)).mapTo[FindAccountsResp].map(_.accounts)
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
        op <- (porter ? DeleteAccount(realm.id, id)).mapTo[OperationFinished]
      } yield "Account deleted: "+ op.result
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
        resp <- (in.porter ? FindAccounts(r.id, Set(login))).mapTo[FindAccountsResp]
        first <- Future.immediate(resp.accounts.headOption, "Account not found")
        op <- (in.porter ? UpdateAccount(r.id, first.changeSecret(Secret.bcryptPassword(passw)))).mapTo[OperationFinished]
      } yield "Password changed: "+ op.result
      in << result
    }
  }

  def manageGroups(verb: String, alter: (Set[Ident], Set[Ident]) => Set[Ident])(implicit ec: ExecutionContext, to: Timeout) = new Form {
    def fields = List("login", "groups")
    def show = {
      case in@Input(msg, conn, _, sess) if msg == (verb+" groups") =>
        in.withRealm { _ =>
          conn ! tcp("Please enter login and groups (separated by space) to add.\n")
        }
        sess.realm.isDefined
    }
    def validateConvert = {
      case ("login", value) => Try(Ident(value))
      case ("groups", value) => Try(Commands.makeList(' ')(value).map(_.trim).toSet.map(Ident.apply))
    }
    def onComplete(in: Input) = {
      val login = in.session[Ident]("login")
      val toadd = in.session[Set[Ident]]("groups")
      val result = for {
        r <- in.realmFuture
        resp <- (in.porter ? FindAccounts(r.id, Set(login))).mapTo[FindAccountsResp]
        first <- Future.immediate(resp.accounts.headOption, "Account not found")
        op <- (in.porter ? UpdateAccount(r.id, first.updateGroups(g => alter(g, toadd)))).mapTo[OperationFinished]
      } yield if (op.result) "Successful" else "Failed."
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

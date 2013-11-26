package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.app.akka.PorterActor.{DeleteAccount, ListAccounts, UpdateAccount}
import porter.model.{Properties, Secret, Ident, Account}
import scala.util.Try

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 26.11.13 00:06
 */
object AccountCommands extends Commands {
  import akka.pattern.ask

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(update, listall, delete)

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
      in.withRealm { r =>
        in.onSuccess(in.porter.ref ? UpdateAccount(r.id, ad.get)) { x =>
          in << "Account created."
        }
      }
    }
  }

  def listall(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @Input(msg, conn, porter, _) if msg == "la" =>
      in.withRealm { r =>
        in.onSuccess((porter.ref ? ListAccounts(r.id)).mapTo[Iterable[Account]]) { list =>
          val groups = (a: Account) => a.groups.map(_.name).mkString("(", ",", ")")
          val props = (a: Account) => a.props.map({case (k,v) => k +"="+v}).mkString("[", ",", "]")
          in << list.map(a => s"${a.name.name} ${groups(a)} ${props(a)}").mkString("\n")
        }
      }
  }

  def delete(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in@Input(msg, conn, porter, _) if msg.startsWith("delete account") =>
      in.withRealm { realm =>
        val name = Try(Ident(msg.substring("delete account".length).trim))
        in.onSuccess(name) { id =>
          in.onSuccess(porter.ref ? DeleteAccount(realm.id, id)) { _ =>
            in << "Account deleted."
          }
        }
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

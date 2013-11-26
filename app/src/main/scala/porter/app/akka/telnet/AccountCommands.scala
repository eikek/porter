package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout
import porter.app.akka.PorterActor.{ListAccounts, UpdateAccount}
import porter.model.{Secret, Ident, Account}
import scala.util.Try

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 26.11.13 00:06
 */
object AccountCommands extends Commands {
  import akka.pattern.ask

  def make(implicit executor: ExecutionContext, to: Timeout) =
    List(update, listall)

  def update(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @ Input(msg, conn, porter, sess) if msg.startsWith("update account") =>
      in.withRealm { r =>
        val ad = AccountDetails()
        sess.put(AccountDetails, ad)
        ad.setnext(AccountDetails.name)
        conn ! tcp("Give account details.\nlogin: ")
      }

    case in @ Input(msg, conn, _, AccountDetails(ad)) if ad.isnext(AccountDetails.name) =>
      ad.put(AccountDetails.name, msg.trim)
      ad.setnext(AccountDetails.password)
      conn ! tcp("password: ")

    case in @ Input(msg, conn, _, AccountDetails(ad)) if ad.isnext(AccountDetails.password) =>
      ad.put(AccountDetails.password, msg.trim)
      ad.setnext(AccountDetails.groups)
      conn ! tcp("groups: ")

    case in @ Input(msg, conn, _, AccountDetails(ad)) if ad.isnext(AccountDetails.groups) =>
      ad.put(AccountDetails.groups, msg.trim)
      ad.setnext(AccountDetails.props)
      conn ! tcp("props: ")

    case in @ Input(msg, conn, porter, AccountDetails(ad)) if ad.isnext(AccountDetails.props) =>
      ad.put(AccountDetails.props, msg.trim)
      if (ad.isComplete) {
        in.session.remove(AccountDetails)
        in.withRealm { r =>
          in.onSuccess(porter.ref ? UpdateAccount(r.id, ad.toAccount.get)) { x =>
            conn ! prompt("Account created.")
          }
        }
      }
  }

  def listall(implicit executor: ExecutionContext, to: Timeout): Command = {
    case in @Input(msg, conn, porter, _) if msg == "la" =>
      in.withRealm { r =>
        in.onSuccess((porter.ref ? ListAccounts(r.id)).mapTo[Iterable[Account]]) { list =>
          conn ! prompt(list.map(a => s"${a.name.name} (${a.groups.map(_.name).mkString(",")})").mkString("\n"))
        }
      }
  }

  object AccountDetails {
    val name = "name"; val groups = "groups"; val password = "password"; val props = "props"
    def unapply(sess: Session): Option[AccountDetails] = sess.get(this) match {
      case Some(ad: AccountDetails) => Some(ad)
      case _ => None
    }
  }
  case class AccountDetails(map: collection.mutable.Map[String, String] = collection.mutable.Map.empty) {
    import AccountDetails._
    private val keys = List(name, groups, password, props)

    def isComplete = keys.forall(k => map.get(k).isDefined)
    def isEmpty = map.isEmpty
    def put(key: String, value: String) = map += (key -> value)
    def setnext(key: String) = map += ("_next" -> key)
    def isnext(key: String) = map.get("_next").exists(p => p == key)

    def toAccount = Try(Account(
      map(name),
      Commands.makePairs(map(props)).get,
      Commands.makeList(map(groups)).toSet.map(Ident.apply),
      Seq(Secret.bcryptPassword(map(password)))
    ))
  }
}

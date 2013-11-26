package porter.app

import porter.auth.PasswordAuthenticator
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.typesafe.config.ConfigFactory
import porter.store.{Store, MutableStore}
import porter.model._
import porter.model.Group
import porter.model.Realm
import porter.model.Account
import scala.concurrent.ExecutionContext

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 00:32
 */
class PorterSettingsTest extends FunSuite with ShouldMatchers {

  test("read config") {
    val cfg = ConfigFactory.parseString(
      """
        |authenticators: [ { class: "porter.auth.PasswordAuthenticator", params: {} } ]
        |stores: [
        |  { class: "porter.app.ConfigStore", params: ${storeconfig}, realms: [] },
        |  { class: "porter.app.TestMStore", params: {}, realms: [ "realm1" ] }
        |]
        |storeconfig: {
        |
        |}
      """.stripMargin)

    val settings = new PorterSettings(cfg.resolve())
    settings.authenticators should have size 1
    settings.authenticators(0) should be (PasswordAuthenticator)
    settings.stores should have size 2
    settings.stores(0).getClass should be (classOf[ConfigStore])
    settings.mutableStores should have size 1
    settings.mutableStores(0)._1 should be (List(Ident("realm1")))
    val Some(s) = settings.findMutableStore(Ident("realm1"))
    s.getClass should be (classOf[TestMStore])
  }
}

class TestMStore extends Store with MutableStore {
  def findRealms(names: Set[Ident])(implicit ec: ExecutionContext) = ???
  def findAccounts(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = ???
  def findAccountsFor(realm: Ident, creds: Set[Credentials])(implicit ec: ExecutionContext) = ???
  def findGroups(realm: Ident, names: Set[Ident])(implicit ec: ExecutionContext) = ???
  def allRealms(implicit ec: ExecutionContext) = ???
  def allAccounts(realm: Ident)(implicit ec: ExecutionContext) = ???
  def allGroups(realm: Ident)(implicit ec: ExecutionContext) = ???
  def updateRealm(realm: Realm)(implicit ec: ExecutionContext) = ???
  def deleteRealm(realm: Ident)(implicit ec: ExecutionContext) = ???
  def updateAccount(realm: Ident, account: Account)(implicit ec: ExecutionContext) = ???
  def deleteAccount(realm: Ident, accId: Ident)(implicit ec: ExecutionContext) = ???
  def updateGroup(realm: Ident, group: Group)(implicit ec: ExecutionContext) = ???
  def deleteGroup(realm: Ident, groupId: Ident)(implicit ec: ExecutionContext) = ???
}

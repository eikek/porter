package porter.auth

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.store.{Store, StoreProvider}
import porter.model._
import porter.model.Account
import scala.util.{Failure, Success}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 02:34
 */
class AuthzTest extends FunSuite with ShouldMatchers {
  val testPw = Secret.bcryptPassword("test")
  val factory = RuleFactory.defaultFactory

  object TestAuth extends AuthZ with RuleFactory with StoreProvider {
    def store = new Store {
      def findRealms(names: Set[Ident]) = Success(List(Realm(Ident.randomIdent, "")))
      def findAccounts(realm: Ident, names: Set[Ident]) =
        Success(List(Account("john",
          Map("email" -> "john@mail.com", "enabled" -> "true"),
          Set("users", "admin"), Seq(testPw))))

      def findAccountsFor(realm: Ident, creds: Set[Credentials]) = ???
      def findGroups(realm: Ident, names: Set[Ident]) = Success(List(
        Group(name = "admin", rules = Set("resource:read:/**", "contact:manage", "resource:read:/main/**"))
      ))
      def allRealms() = ???
      def allAccounts(realm: Ident) = ???
      def allGroups(realm: Ident) = ???
    }
  }

  test("get policy") {
    val pol = TestAuth.getPolicy(Ident.randomIdent)("john").get
    pol.rules should have size (2)
    pol.grantsAll(List(factory("resource:read:/hello.html"))) should be (true)
    pol.grantsAll(List(factory("contact:delete"))) should be (false)
  }

  test("authorized") {
    val authz = TestAuth.authorized(Ident.randomIdent)_
    authz("john", List(factory("resource:read:/test.pdf"))) should be (true)
    authz("john", List(factory("contact:delete"))) should be (false)

    val Failure(_) = TestAuth.createRule("resource:read/test")
  }
}

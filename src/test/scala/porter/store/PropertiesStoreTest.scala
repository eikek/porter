package porter.store

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.util.Success

/**
 *
 * @since 22.11.13 20:30
 *
 */
class PropertiesStoreTest extends FunSuite with ShouldMatchers {
  import porter.model._

  val testPw = Secret.bcryptPassword("test")
  val acc = Account("john", Map("email" -> "john@mail.com", "enabled" -> "true"), Set("users", "admin"), Seq(testPw))

  private def createProps() = {
    val props = new java.util.Properties()
    props.setProperty("porter.realms", "app1")
    props.setProperty("porter.app1.name", "My Realm")
    props.setProperty("porter.app1.accounts", "john")
    props.setProperty("porter.app1.account.john.secret", testPw.asString)
    props.setProperty("porter.app1.account.john.groups", "admin, users")
    props.setProperty("porter.app1.account.john.props", "email -> john@mail.com, enabled->true")
    props.setProperty("porter.app1.groups", " admin,  users ")
    props.setProperty("porter.app1.group.admin.permissions", "resource:read:/main/**, base:manage")
    props
  }

  private def createProps(values: Map[String, String]) = {
    val props = new java.util.Properties()
    for ((k, v) <- values) props.setProperty(k, v)
    props
  }

  test("list realms") {
    val store = new PropertiesStore(createProps(Map(
      "porter.realms" -> "app1,app2,app3",
      "porter.app1.name" -> "A realm 1",
      "porter.app2.name" -> "A realm 2",
      "porter.app3.name" -> "A realm 3",
      "porter.app4.name" -> "A realm 4"
    )))
    store.allRealms() should be (Success(List(Realm("app1", "A realm 1"),
      Realm("app2", "A realm 2"), Realm("app3", "A realm 3"))))
  }

  test("find realms") {
    val store = new PropertiesStore(createProps())
    store.findRealms(Set("app1")) should be (Success(List(Realm("app1", "My Realm"))))
    store.findRealms(Set("asdasd")) should be (Success(List()))
  }

  test("list groups") {
    val store = new PropertiesStore(createProps())
    store.allGroups("app1") should be (Success(List(
      Group(name = "admin", rules = Set("resource:read:/main/**", "base:manage")), Group("users"))))
  }

  test ("find groups") {
    val store = new PropertiesStore(createProps())
    store.findGroups("app1", Set("users")) should be (Success(List(Group("users"))))
  }

  test ("list accounts") {
    val store = new PropertiesStore(createProps())
    store.allAccounts("app1").get should be (List(acc))
  }

  test ("find accounts") {
    val store = new PropertiesStore(createProps())
    store.findAccounts("app1", Set("john")).get should be (List(acc))
  }
}

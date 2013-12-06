package porter.app.akka.api

import porter.store.SimpleStore

object TestData {
  import porter.model._

  val testpassword = Secret.bcryptPassword("test")

  val john = Account(
    name = "john",
    groups = Set("g1", "g2"),
    secrets = Seq(testpassword)
  )
  val policyJohn = Policy(Set(
    DefaultPermission("perm1:x"),
    DefaultPermission("perm1:y:1"),
    ResourcePermission("resource:read:/**")))

  val mary = Account(
    name = "mary",
    groups = Set("g2", "g4"),
    secrets = Seq(testpassword)
  )

  val g1 = Group(name = "g1", rules = Set("perm1:x:1", "perm1:y:1", "resource:read:/main/**"))
  val g2 = Group(name = "g2", rules = Set("perm1:x", "resource:read:/**"))
  val g3 = Group(name = "g3", rules = Set("perm1:y", "resource:read:/**"))
  val g4 = Group(name = "g4", rules = Set("perm1", "resource:read:/**"))

  val realm = Realm("r1", "Test Realm")

  val store1 = createStore(
    realms = List(realm),
    groups = List(realm -> g1, realm -> g2),
    accounts = List(realm -> john, realm -> mary)
  )
  val store2 = createStore(
    realms = List(realm, Realm("r4", "")),
    groups = List(realm -> g3, realm -> g4),
    accounts = List(realm -> Account("james"), realm -> Account("gloria"))
  )
  
  def createStore(realms: Iterable[Realm] = Set(),
                  accounts: Iterable[(Realm, Account)] = Set(),
                  groups: Iterable[(Realm, Group)] = Set()) = SimpleStore(realms, groups, accounts)
  
}

package porter.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import porter.auth.RuleFactory

class PolicyTest extends FunSuite with ShouldMatchers {

  val policy = new Policy(Set(
    Permission("read:a:b:*"), //same as read:a:b
    Permission("read:z"),
    Permission("read:a:b:x").revoke
  ))

  test("grantsAll") {
    policy grantsAll List(Permission("read:a:b:c")) should be (true)
    policy grantsAll List(Permission("read:a:b:x:test")) should be (false)
    policy grantsAll List(Permission("read:z")) should be (true)
    policy grantsAll List(Permission("read:z:a")) should be (true)

    policy grantsAll List(Permission("read:z:a"), Permission("read:x:a")) should be (false)
  }

  test("reduce policy") {
    val fac = ResourcePermission.factory
    val policy = new Policy(Set(
      fac("resource:read:/**"),
      fac("resource:read:/main/**")
    ))
    policy.rules should have size (1)
  }

  test("reduce policy and revocations") {
    val fac = RuleFactory.providedFactory
    Policy(Set(fac("resource:read:/a/b/**"), fac("resource:read:/a/**").revoke)).rules should have size (0)
    Policy(Set(fac("a:b:*"), fac("a:x"), fac("a:x:1:n"), fac("a:x:1").revoke)).rules should have size (3)
  }
}

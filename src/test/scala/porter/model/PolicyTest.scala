package porter.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 *
 * @since 22.11.13 19:27
 *
 */
class PolicyTest extends FunSuite with ShouldMatchers {

  val policy = new Policy(Set(
    Permission("read:a:b:*"), //same as read:a:b
    Permission("read:z"),
    Revocation(Permission("read:a:b:x"))
  ))

  test("grantsAll") {
    policy grantsAll List(Permission("read:a:b:c")) should be (true)
    policy grantsAll List(Permission("read:a:b:x:test")) should be (false)
    policy grantsAll List(Permission("read:z")) should be (true)
    policy grantsAll List(Permission("read:z:a")) should be (true)

    policy grantsAll List(Permission("read:z:a"), Permission("read:x:a")) should be (false)
  }
}

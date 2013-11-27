package porter.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 22:41
 */
class ResourcePermissionTest extends FunSuite with ShouldMatchers {

  val factory = ResourcePermission.factory

  test("simple implies") {
    val rp0 = factory("resource:read,write:/manager/**")
    val rp1 = factory("resource:read,write:/manager/html/**")
    val rp2 = factory("resource:read:/manager/test.html")
    val rp3 = factory("resource:delete:/manager/test.html")
    rp0.implies(rp1) should be (true)
    rp1.implies(rp0) should be (false)
    rp1 implies rp2 should be (false)
    rp0 implies rp2 should be (true)
    rp0 implies rp3 should be (false)
    rp3 implies rp3 should be (true)

    val rp4 = factory("resource:read:/**")
    val rp5 = factory("resource:read:/main/**")
    rp4 implies rp5 should be (true)
    rp5 implies rp4 should be (false)
  }

  test("implies where actions=*") {
    val rp0 = factory("resource:*:/main/**")
    val p1 = factory("resource:read:/test.html")
    val p2 = factory("resource:read:/xa/test.html")
    rp0 implies p1 should be (false)
    rp0 implies p2 should be (false)
  }
}

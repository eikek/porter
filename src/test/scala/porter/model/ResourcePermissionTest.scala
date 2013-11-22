package porter.model

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 21.11.13 22:41
 */
class ResourcePermissionTest extends FunSuite with ShouldMatchers {

  test("simple implies") {
    val rp0 = ResourcePermission("read,write:/manager/**")
    val rp1 = ResourcePermission("read,write:/manager/html/**")
    val rp2 = ResourcePermission("read:/manager/test.html")
    val rp3 = ResourcePermission("delete:/manager/test.html")
    rp0.implies(rp1) should be (true)
    rp1.implies(rp0) should be (false)
    rp1 implies rp2 should be (false)
    rp0 implies rp2 should be (true)
    rp0 implies rp3 should be (false)
    rp3 implies rp3 should be (true)
  }
}

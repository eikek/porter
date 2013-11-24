package porter.app.akka

import scala.util.Success
import porter.auth.PasswordAuthenticator
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.typesafe.config.ConfigFactory

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 24.11.13 00:32
 */
class ConfiguredPorterTest extends FunSuite with ShouldMatchers {

  test("read config") {
    val cfg = ConfigFactory.parseString(
      """
        |authenticators: [ { class: "porter.auth.PasswordAuthenticator", params: {} } ]
        |stores: [
        |  { class: "porter.akka.ConfigStore", params: ${storeconfig}, realms: [] }
        |]
        |storeconfig: {
        |
        |}
      """.stripMargin)

    val Success(settings) = ConfiguredPorter.fromConfig(cfg.resolve(), getClass.getClassLoader)
    settings.authenticators should have size 1
    settings.authenticators(0) should be (PasswordAuthenticator)
    settings.stores should have size 1
    settings.stores(0).getClass should be (classOf[ConfigStore])
  }
}

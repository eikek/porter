package porter.app.client.spray

import scala.concurrent.duration._

case class CookieSettings(cookieKey: Vector[Byte],
                          persistAge: Option[Duration] = None,
                          sessionAge: Option[Duration] = Some(1.days),
                          cookiePath: String = "/",
                          cookieName: String = "PORTER",
                          cookieSecure: Boolean = true)

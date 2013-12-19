package porter.app.openid

import porter.model.Ident
import spray.http.Uri
import java.nio.file.Files
import scala.util.Try
import porter.app.openid.common.Supplier

trait OpenIdServiceSettings {

  def decider: porter.auth.Decider

  def contact: Option[String]

  def staticResourceDir: java.nio.file.Path
  def templateDir: java.nio.file.Path

  def cookieName: String
  def cookieKey: Vector[Byte]

  def realms: List[Ident]
  def defaultRealm: Ident
  final def acceptRealm(realm: Ident) = realm == defaultRealm || realms.contains(realm)

  def endpointBaseUrl: Uri
  final def endpointUrl: Uri = endpointBaseUrl.withPath(endpointBaseUrl.path + "/openid/ep")
  final def openIdUrl: Uri = endpointBaseUrl.withPath(endpointBaseUrl.path + "/openid")

  def loadTemplate(name: String): Option[Supplier] = {
    val tfile = templateDir.resolve(name)
    if (Files.isRegularFile(tfile) && Files.isReadable(tfile))
      Some(() => Try(Files.newInputStream(tfile)))
    else {
      val url = Option(getClass.getResource("/porter/app/openid/assets/" + name))
      url.map(u => () => Try(u.openStream()))
    }
  }
}

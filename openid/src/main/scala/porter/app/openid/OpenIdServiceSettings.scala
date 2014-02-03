/*
 * Copyright 2014 porter <https://github.com/eikek/porter>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porter.app.openid

import porter.model.Ident
import spray.http.Uri
import java.nio.file.{Path, Files}
import scala.util.Try
import porter.app.openid.common.Supplier
import porter.model.PasswordCrypt
import org.eknet.spray.openid.provider.Mustache

trait OpenIdServiceSettings {

  def passwordCrypt: PasswordCrypt
  def decider: porter.auth.Decider

  def contact: Option[String]

  def staticResourceDir: java.nio.file.Path
  def templateDir: java.nio.file.Path

  def cookieName: String
  def cookieKey: Vector[Byte]
  def cookieSecure: Boolean

  def realms: List[Ident]
  def defaultRealm: Ident
  final def acceptRealm(realm: Ident) = realm == defaultRealm || realms.contains(realm)

  def endpointBaseUrl: Uri
  final def endpointUrl: Uri = endpointBaseUrl.withPath(endpointBaseUrl.path + "/openid/ep")
  final def openIdUrl: Uri = endpointBaseUrl.withPath(endpointBaseUrl.path + "/openid")

  def registrationEnabled: Boolean
  def registrationRequiresEmail: Boolean
  def registrationKey: Option[String]

  def avatarCacheDir: Option[Path]
  def avatarCacheDirSize: Int
  def avatarMaxUploadSize: Int

  private def loadTemplateFile(name: String): Option[Supplier] = {
    val tfile = templateDir.resolve(name)
    if (Files.isRegularFile(tfile) && Files.isReadable(tfile))
      Some(() => Try(Files.newInputStream(tfile)))
    else {
      val url = Option(getClass.getResource("/porter/app/openid/assets/" + name))
      url.map(u => () => Try(u.openStream()))
    }
  }
  private def loadTemplate(name: String) = {
    val in = loadTemplateFile(name)
    in.map(s => Mustache(s().get)).getOrElse(sys.error(s"$name not found"))
  }

  lazy val loginTemplate = loadTemplate("login-template.mustache")
  lazy val errorTemplate = loadTemplate("error-template.mustache")
  lazy val continueTemplate = loadTemplate("continue-template.mustache")
  lazy val userTemplate = loadTemplate("user-template.mustache")
  lazy val registerTemplate = loadTemplate("register-template.mustache")
}

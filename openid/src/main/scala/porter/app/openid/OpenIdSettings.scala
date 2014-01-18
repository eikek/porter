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

import com.typesafe.config.Config
import scala.util.Try
import java.nio.file.{Path, Paths}
import akka.actor._
import porter.util.{AES, Base64}
import spray.http.Uri
import porter.model.{PasswordCrypt, Ident}

class OpenIdSettings(cfg: Config, da: DynamicAccess) extends Extension with OpenIdServiceSettings {
  import collection.JavaConverters._

  val bindingHost = Try(cfg.getString("host")).getOrElse("localhost")
  val bindingPort = Try(cfg.getInt("port")).getOrElse(8888)

  val passwordCrypt = {
    val config = cfg.getString("password-crypt")
    PasswordCrypt(config).getOrElse(sys.error(s"Invalid configuration for password-crypt: $config"))
  }

  val decider = da.getObjectFor[porter.auth.Decider](cfg.getString("decider")).get

  val contact = Try(cfg.getString("contact")).toOption.filter(_.nonEmpty)

  val staticResourceDir = Paths.get(cfg.getString("static-resource-dir"))
  val templateDir = Paths.get(cfg.getString("template-dir"))

  val cookieName = cfg.getString("cookie-name")
  val cookieKey = Try(cfg.getString("cookie-key")).map(Base64.decode).getOrElse(AES.generateRandomKey).toVector
  val cookieSecure = cfg.getBoolean("cookie-secure")

  val realms = cfg.getStringList("realms").asScala.toList.map(Ident.apply)

  val defaultRealm = Ident(Try(cfg.getString("default-realm")).getOrElse("default"))

  val endpointBaseUrl = Try(cfg.getString("endpoint-base-url")).map(Uri.apply).getOrElse {
    val host = if (bindingHost == "0.0.0.0") "localhost" else bindingHost
    Uri(s"http://$host:$bindingPort")
  }

  val registrationEnabled = cfg.getBoolean("registration-enabled")
  val registrationRequiresEmail = cfg.getBoolean("registration-requires-email")
  val registrationKey = Try(cfg.getString("registration-invitation-key")).toOption.filter(_.trim.nonEmpty)

  val avatarCacheDir: Option[Path] = Try(cfg.getString("avatar-cache-dir")).toOption.filter(_.trim.nonEmpty).map(Paths.get(_))
  val avatarCacheDirSize: Int = Try(cfg.getString("avatar-cache-dir-size").toInt).toOption.getOrElse(20)
  val avatarMaxUploadSize: Int = cfg.getInt("avatar-max-upload-size")
}

object OpenIdSettings extends ExtensionId[OpenIdSettings] with ExtensionIdProvider {
  def lookup() = OpenIdSettings

  def createExtension(system: ExtendedActorSystem) =
    new OpenIdSettings(system.settings.config.getConfig("porter.openid"), system.dynamicAccess)
}
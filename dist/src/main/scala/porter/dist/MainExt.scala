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

package porter.dist

import akka.actor._
import porter.dist.MainExt.{HttpSettings, InterfaceSettings}
import porter.app.openid.OpenIdSettings
import porter.auth.Decider
import porter.model.PasswordCrypt

object MainExt extends ExtensionId[MainExt] with ExtensionIdProvider {
  def lookup() = MainExt

  def createExtension(system: ExtendedActorSystem) = MainExt(system)

  case class InterfaceSettings(host: String, port: Int, enabled: Boolean)
  case class HttpSettings(iface: InterfaceSettings, decider: Decider)
}

case class MainExt(private val system: ExtendedActorSystem) extends Extension {

  val address = system.provider.getDefaultAddress

  def pathFor(porter: ActorRef) = {
    ActorPath.fromString(address.toString + porter.path.elements.mkString("/", "/", ""))
  }

  def isRemote = address.host != None

  private val config = system.settings.config.getConfig("porter")
  val openidSettings = new OpenIdSettings(config.getConfig("openid"), system.dynamicAccess)

  val telnet = InterfaceSettings(
    config.getString("telnet.host"),
    config.getInt("telnet.port"),
    config.getBoolean("telnet.enabled")
  )

  val http = HttpSettings(
    InterfaceSettings(
      config.getString("http.host"),
      config.getInt("http.port"),
      config.getBoolean("http.enabled")
    ),
    system.dynamicAccess.getObjectFor[porter.auth.Decider](config.getString("http.decider")).get
  )

  val openid = InterfaceSettings(
    config.getString("openid.host"),
    config.getInt("openid.port"),
    config.getBoolean("openid.enabled")
  )
}

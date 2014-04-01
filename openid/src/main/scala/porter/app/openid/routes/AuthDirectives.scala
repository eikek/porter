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

package porter.app.openid.routes

import porter.model._
import akka.util.Timeout
import spray.routing._
import porter.util.Hash
import scala.Some
import porter.model.Account
import porter.model.Property.BoolProperty
import porter.app.client.spray.{CookieSettings, PorterDirectives}
import porter.app.client.PorterContext

trait AuthDirectives extends BasicDirectives with PorterDirectives {
  self: OpenIdActors =>

  import scala.concurrent.duration._

  private def porterContext(realm: Ident) = PorterContext(porterRef, realm, settings.decider)
  private def cookieSettings(maxAge: Option[Duration]) = {
    val cookiePath = settings.endpointBaseUrl.toRelative.toString()
    CookieSettings(settings.cookieKey, maxAge, Some(1.days), cookiePath, settings.cookieName, settings.cookieSecure)
  }

  def cookieCredentials: Directive1[Set[Credentials]] =
    PorterDirectives.cookieCredentials(settings.cookieKey, settings.cookieName)

  def credentials: Directive1[Set[Credentials]] = cookieCredentials ++ formCredentials

  def emptyCredentials: Directive0 = credentials.isEmpty

  def setAuthCookie(account: Account, maxAge: Option[Duration] = None): Directive0 = {
    setPorterCookie(account, cookieSettings(maxAge))
  }

  def removePorterCookie() = deleteCookie(settings.cookieName)

  def setPorterCookieOnRememberme(account: Account) =
    (checkboxActive("porter.rememberme") & setAuthCookie(account, Some(10.days))) | setAuthCookie(account)

  val rememberRealmPropName = "porter-openid-rememberRealm"
  def rememberRealmProperty(realmUri: String) =
    BoolProperty(rememberRealmPropName +"-"+ Hash.md5String(realmUri))

  def authc: Directive1[Account] = credentials.flatMap { creds =>
    authenticateAccount(porterContext(settings.defaultRealm), creds).flatMap { account =>
      setPorterCookieOnRememberme(account).hflatMap { _ =>
        provide(account)
      }
    }
  }
}

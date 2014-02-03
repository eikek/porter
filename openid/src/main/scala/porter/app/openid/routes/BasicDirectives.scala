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

import spray.routing._
import spray.http._
import spray.routing.Directives._
import porter.model.Ident
import scala.util.Try
import java.util.Locale
import porter.model.PropertyList._
import scala.Some
import porter.model.Account
import scala.util.Success

trait BasicDirectives extends Directives {

  def requestLocale: Directive1[Locale] = headerValuePF {
    case HttpHeaders.`Accept-Language`(lang) if lang.nonEmpty =>
      Locale.forLanguageTag(lang.head.toString())
  }

  def accountLocale(account: Account): Directive1[Locale] = {
    val loc = for {
      accloc <- locale.get(account.props)
      loc <- Try(Locale.forLanguageTag(accloc)).toOption
    } yield loc
    loc.map(provide).getOrElse(reject)
  }

  def localeWithDefault(acc: Option[Account] = None): Directive1[Locale] =
    acc match {
      case Some (a) => accountLocale(a) | requestLocale | provide(Locale.getDefault)
      case _ => requestLocale | provide(Locale.getDefault)
    }

  def checkboxActive(name: String): Directive0 =
    formField(name.?).flatMap(x => if (x.exists(_ equalsIgnoreCase "on")) pass else reject())
}

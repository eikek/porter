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

import akka.actor.ActorRef
import spray.http.{FormData, Uri}
import spray.routing._
import org.eknet.spray.openid.model.CheckIdRequest
import org.eknet.spray.openid.model.SimpleRegistration.Field
import org.eknet.spray.openid.provider.{SRegExtension, AccountIdentity, ProviderHooks}
import org.eknet.spray.openid.provider.MustacheContext.KeyedData
import porter.app.openid.routes.{OpenIdActors, AuthDirectives}
import porter.model._
import porter.client.messages._
import porter.app.openid.common.LocalId

trait OpenIdProviderHook extends AuthDirectives with Directives {
  self: OpenIdActors =>

  def createHook: ProviderHooks = new Impl(porterRef, settings)

  private def accountIdentity(settings: OpenIdServiceSettings) = new AccountIdentity[Account] {
    private val delegate = AccountIdentity.urlSuffix(settings.endpointBaseUrl.toString())
    def fromIdentityUrl(url: String) = delegate.fromIdentityUrl(url).substring(1)
    def toIdentityUrl(a: Account) = delegate.toIdentityUrl(a.name.name)
  }

  private val sregPropertyMapping = Map(
    Field.fullname -> PropertyList.fullName,
    Field.email -> PropertyList.email,
    Field.language -> PropertyList.locale,
    Field.timezone -> PropertyList.timezone,
    Field.country -> PropertyList.country
  )
  private def defaultValues(account: Account): Map[String, String] = {
    (for {
      (f, p) <- sregPropertyMapping
      pv <- p.get(account.props).toList
    } yield f.name -> pv).toMap
  }

  private class Impl(porterRef: ActorRef, settings: OpenIdServiceSettings) extends
    ProviderHooks.SimpleHooks(settings.loginTemplate, settings.continueTemplate, accountIdentity(settings))(authc) {
    import akka.pattern.ask
    import porter.app.openid.routes.Templating._

    override def renderConfirmationPage(req: CheckIdRequest, a: Account, endpoint: Uri) = {
      val areq = req.supplement(Map("porter.account" -> a.name.name, "porter.realm" -> settings.defaultRealm.name))
      val context = requestContext(areq)
        .andThen(KeyedData("endpointUrl").put(endpoint))
        .andThen(SRegExtension.formFields(areq, defaultValues(a)))
      renderConfirm(context(defaultContext(settings)))
    }

    override def renderLoginPage(req: CheckIdRequest, endpoint: Uri) = {
      formField("spray-openid.submitType".?) { st =>
        val context = loginPageContext(req, st, endpoint)
        removePorterCookie() {
          renderLogin(context(defaultContext(settings)))
        }
      }
    }

    override def isUserSubmit = super.isUserSubmit.hflatMap { _ =>
      rememberContinueSubmit
    }

    override def skipConfirmation(a: Account) = {
      isContinueRemembered(a)
    }

    /**
     * Passes, if the realm of the current openid request is remembered
     * in the account properties
     * @param account
     * @return
     */
    private def isContinueRemembered(account: Account): Directive0 = {
      anyParam("openid.realm").flatMap { realmuri =>
        if (rememberRealmProperty(realmuri).isTrue(account.props)) pass
        else reject()
      }
    }

    /**
     * Starts a future that will store the given realm-uri into the
     * properties of the given account
     * @param localId
     * @param realmUri
     */
    private def updateContinueRemembered(localId: LocalId, realmUri: String) {
      val f = for {
        acc <- (porterRef ? FindAccounts(localId.realm, Set(localId.account))).mapTo[FindAccountsResp]
        if acc.accounts.nonEmpty
        upd <- (porterRef ? UpdateAccount(localId.realm, acc.accounts.head.updatedProps(rememberRealmProperty(realmUri).set(true)))).mapTo[OperationFinished]
      } yield upd
      f onFailure { case x =>
        log.error(x, "Unable to remember continue decision")
      }
    }

    /**
     * Always passes and starts an operation before, that will remember the
     * realm of the current relying party, if the user has chosen so.
     * @return
     */
    private def rememberContinueSubmit: Directive0 = {
      entity(as[FormData]).flatMap { data =>
        val params = data.fields.toMap
        if (params.get("porter.rememberContinue").exists(_ equalsIgnoreCase "on")) {
          (params.get("porter.account"), params.get("porter.realm"), params.get("openid.realm")) match {
            case (Some(a), Some(r), Some(uri)) => updateContinueRemembered(LocalId(r, a), uri)
            case _ =>
          }
        }
        pass
      }
    }
  }

}

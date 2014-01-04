package porter.app.openid.routes

import porter.model._
import akka.util.Timeout
import spray.routing._
import spray.http.Uri
import scala.io.Codec
import porter.util.{Hash, Base64}
import scala.util.Failure
import scala.Some
import porter.model.Account
import scala.util.Success
import porter.app.openid.AssocActor.AssocToken
import porter.model.Property.BoolProperty
import porter.app.client.spray.{CookieSettings, PorterContext, PorterDirectives}

trait AuthDirectives extends AssociationDirectives {
  self: OpenIdActors =>

  import porter.app.openid.common._
  import scala.concurrent.duration._
  import spray.routing.Directives._
  import PorterDirectives.authenticateAccount
  import PorterDirectives.setPorterCookie
  import PorterDirectives.formCredentials
  import PorterDirectives.CredentialsConcat

  private val successParams = Set(
    Keys.ns, Keys.mode, Keys.op_endpoint, Keys.claimed_id, Keys.identity, Keys.return_to,
    Keys.response_nonce, Keys.invalidate_handle, Keys.assoc_handle, Keys.sig, Keys.signed
  ).map(_.openid)
  private val requiredSignedKeys: List[Key] = List(Keys.op_endpoint, Keys.return_to,
    Keys.response_nonce, Keys.assoc_handle, Keys.claimed_id, Keys.identity)

  private def porterContext(realm: Ident) = PorterContext(porterRef, realm, settings.decider)
  private def cookieSettings(maxAge: Option[Duration]) = {
    val cookiePath = settings.endpointBaseUrl.path match {
      case Uri.Path.Empty => "/"
      case p => p.toString()
    }
    CookieSettings(settings.cookieKey, maxAge, Some(1.days), cookiePath, settings.cookieName, settings.cookieSecure)
  }

  def authcAccount(realm: Ident, creds: Set[Credentials])(implicit timeout: Timeout): Directive1[Account] =
    authenticateAccount(porterContext(realm), creds)

  def authcToken(realm: Ident, creds: Set[Credentials])(implicit timeout: Timeout): Directive1[Authenticated] =
    paramOpt(Keys.assoc_handle.openid).flatMap { handle =>
      authcAccount(realm, creds).flatMap { account =>
        onComplete(associationFuture(handle)).flatMap {
          case Success(assoc) => provide(Authenticated(account, assoc))
          case Failure(ex) => reject()
          case _ => reject()
        }
      }
    }

  def cookieCredentials: Directive1[Set[Credentials]] =
    PorterDirectives.cookieCredentials(settings.cookieKey, settings.cookieName)

  def credentials: Directive1[Set[Credentials]] = cookieCredentials ++ formCredentials

  def noCredentials: Directive0 = credentials.isEmpty

  def setAuthCookie(account: Account, maxAge: Option[Duration] = None): Directive0 = {
    setPorterCookie(account, cookieSettings(maxAge))
  }

  def removePorterCookie() = deleteCookie(settings.cookieName)

  def setPorterCookieOnRememberme(account: Account) =
    (paramIs("porter.rememberme", _.equalsIgnoreCase("on")) & setAuthCookie(account, Some(10.days))) | setAuthCookie(account)

  def positiveAssertion(auth: Authenticated, realm: Ident): Directive1[Map[String, String]] = allParams.flatMap {
    case params =>
      val path =
        if (realm == settings.defaultRealm) Uri.Path("/"+auth.account.name.name)
        else Uri.Path("/"+ realm.name +"/"+ auth.account.name.name)
      val id = settings.endpointBaseUrl.withPath(path).toString()
      val incl = params.filter { case (k, v) => successParams contains k }  ++ Map(
        Keys.mode.openid -> "id_res",
        Keys.realm.openid -> params.get(Keys.realm.openid).getOrElse(""),
        Keys.response_nonce.openid -> nextNonce,
        Keys.op_endpoint.openid -> settings.endpointUrl.toString(),
        Keys.assoc_handle.openid -> auth.association.handle,
        Keys.claimed_id.openid -> id,
        Keys.identity.openid -> id
      )
      params.get(Keys.assoc_handle.openid) match {
        case Some(v) if auth.association.token.priv =>
          provide(signedResponse(auth.association.token)(incl.updated(Keys.invalidate_handle.openid, v)))
        case _ =>
          provide(signedResponse(auth.association.token)(incl))
      }
  }

  def signedResponse(token: AssocToken)(params: Map[String, String]) = {
    val fields = requiredSignedKeys.filter(k => params contains k.openid)
    val values = fields map { k => s"${k.name}:${params(k.openid)}\n" }
    val data = values.mkString("").getBytes(Codec.UTF8.charSet).toVector
    val sig = Base64.encode(token.assocType.crypt.sign(token.mac, data).get)
    params.updated(Keys.sig.openid, sig)
      .updated(Keys.signed.openid, fields.map(k => k.name).mkString(","))
  }

  def validateResponse(token: AssocToken, params: Map[String, String]): Boolean = {
    val sig = signedResponse(token)(params)
    (sig.get(Keys.sig.openid), params.get(Keys.sig.openid)) match {
      case (Some(x), Some(y)) if x == y => true
      case _ => false
    }
  }

  val rememberRealmPropName = "porter-openid-rememberRealm"
  def rememberRealmProperty(realmUri: String) =
    BoolProperty(rememberRealmPropName +"-"+ Hash.md5String(realmUri))
}

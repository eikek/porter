package porter.app.openid.routes

import porter.model._
import porter.app.openid._
import akka.util.Timeout
import spray.routing._
import shapeless.HNil
import spray.http.{Uri, HttpCookie}
import scala.io.Codec
import porter.util.Base64
import porter.auth.AuthResult
import scala.util.Failure
import porter.app.akka.api.StoreActor.messages.FindAccountsResp
import scala.Some
import porter.app.akka.api.AuthcWorker.messages.Authenticate
import porter.app.openid.AssocActor.AssocToken
import porter.app.akka.api.AuthcWorker.messages.AuthenticateResp
import scala.util.Success
import porter.app.akka.api.StoreActor.messages.FindAccounts
import java.util.{TimeZone, Date, UUID}
import porter.auth.AuthResult
import scala.util.Failure
import scala.Some
import porter.model.Account
import porter.app.akka.api.AuthcWorker.messages.AuthenticateResp
import scala.util.Success
import porter.app.akka.api.StoreActor.messages.FindAccountsResp
import porter.app.akka.api.AuthcWorker.messages.Authenticate
import porter.app.openid.AssocActor.AssocToken
import porter.app.akka.api.StoreActor.messages.FindAccounts

trait AuthDirectives extends AssociationDirectives {
  import _root_.porter.app.openid.common._
  import scala.concurrent.duration._
  import shapeless.HList.ListCompat._
  import spray.routing.Directives._
  import akka.pattern.ask

  private val successParams = Set(
    Keys.ns, Keys.mode, Keys.op_endpoint, Keys.claimed_id, Keys.identity, Keys.return_to,
    Keys.response_nonce, Keys.invalidate_handle, Keys.assoc_handle, Keys.sig, Keys.signed
  ).map(_.openid)
  private val requiredSignedKeys: List[Key] = List(Keys.op_endpoint, Keys.return_to,
    Keys.response_nonce, Keys.assoc_handle, Keys.claimed_id, Keys.identity)

  private def nextNonce = {
    val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))
    df.format(new java.util.Date()) + UUID.randomUUID().toString.replace('-', 'z')
  }

  def authFuture(creds: Set[Credentials], realm: Ident)(implicit timeout: Timeout) = {
    (porter ? Authenticate(realm, creds)).mapTo[AuthenticateResp].map {
      case AuthenticateResp(Some(r), _) if settings.decider(r) => r
      case _ => sys.error("Invalid Credentials")
    }
  }
  def accountFuture(lid: LocalId)(implicit timeout: Timeout) = {
    (porter ? FindAccounts(lid.realm, Set(lid.account))).mapTo[FindAccountsResp].map {
      case FindAccountsResp(a, _) => a.headOption
      case _ => None
    }
  }

  def authenticate(creds: Set[Credentials], realm: Ident)(implicit timeout: Timeout): Directive1[AuthResult] = {
    onComplete(authFuture(creds, realm)).hflatMap {
      case Success(result) #: HNil => provide(result)
      case Failure(x) #: HNil => reject()
    }
  }

  def account(lid: LocalId)(implicit timeout: Timeout): Directive1[Account] = {
    onComplete(accountFuture(lid)).hflatMap {
      case Success(Some(a)) #: HNil => provide(a)
      case _ => reject()
    }
  }

  def authenticateToken(creds: Set[Credentials], realm: Ident)(implicit timeout: Timeout): Directive1[Authenticated] =
    paramOpt(Keys.assoc_handle.openid).flatMap { handle =>
      val f = for {
        auth <- authFuture(creds, realm)
        account <- accountFuture(LocalId(auth.realm.id, auth.accountId))
        assoc <- associationFuture(handle)
      } yield (account, assoc)
      onComplete(f).flatMap {
        case Success((Some(acc), assoc)) => provide(Authenticated(acc, assoc))
        case Failure(ex) => ex.printStackTrace(); reject()
        case _ => reject()
      }
    }

  def cookieCredentials: Directive1[Set[Credentials]] = localIdOption.hflatMap { rname =>
    val login = rname.head.map(_.account)
    optionalCookie(settings.cookieName).map {
      case Some(c) =>
        val derived = DerivedCredentials.tryDecode(settings.cookieKey, c.content).toOption.
          filter(dc => login.isEmpty || dc.accountName == login.get)
        derived.map(Set[Credentials](_)).getOrElse(Set.empty[Credentials])
      case _ => Set.empty[Credentials]
    }
  }


  def userpassCredentials: Directive1[Set[Credentials]] =
    formField("porter.username").flatMap(un =>
      formField("porter.password").map(pw =>
        Set[Credentials](PasswordCredentials(un, pw)))) | provide(Set.empty[Credentials])

  def credentials: Directive1[Set[Credentials]] =
    cookieCredentials.hflatMap(cs => userpassCredentials.hmap(ps => cs.head++ps.head::HNil))

  def noCredentials: Directive0 = credentials.hflatMap {
    case s #: HNil if s.nonEmpty => reject()
    case _ => pass
  }


  def setPorterCookie(account: Account): Directive0 = {
    def cookiePath = settings.endpointBaseUrl.path match {
      case Uri.Path.Empty => "/"
      case p => p.toString()
    }
    optionalCookie(settings.cookieName).flatMap {
      case Some(c) if DerivedCredentials.tryDecode(settings.cookieKey, c.content).isSuccess => pass
      case _ => (paramIs("porter.rememberme", "on") & {
        val data = DerivedCredentials(account.name, account.secrets.head, 10.days).encode(settings.cookieKey)
        setCookie(HttpCookie(name = settings.cookieName,
          content = data,
          httpOnly = true,
          path = Some(cookiePath)))
      }) | pass

    }
  }

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
}

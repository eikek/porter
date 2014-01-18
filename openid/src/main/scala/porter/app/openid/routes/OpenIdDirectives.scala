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

trait OpenIdDirectives {
  self: OpenIdActors =>

  import _root_.porter.app.openid.common._
  import Implicits._
  import spray.httpx.unmarshalling._
  import shapeless.HList.ListCompat._

  def openIdEndpoint = PathMatchers.separateOnSlashes(settings.endpointUrl.path.dropChars(1).toString())

  def returnToUrl: Directive1[Uri] = param(Keys.return_to.openid).flatMap { rto =>
    param(Keys.realm.openid).flatMap { realm =>
      Try(Uri(rto)) match {
        case Success(uri) if uri.matchesRealm(realm) => provide(uri)
        case _ => reject()
      }
    }
  }

  def redirectToRelyingParty(params: Map[String, String]) = returnToUrl { returnto =>
    redirect(returnto.withQuery(params ++ returnto.query.toMap), StatusCodes.Found)
  }

  def param(name: String): Directive1[String] = anyParam(name)
  def paramOpt(name: String): Directive1[Option[String]] = param(name).map(Some.apply) | provide(None)

  def paramIs(name: String, pred: String => Boolean): Directive0 = paramOpt(name).flatMap {
    case Some(v) => if (pred(v)) pass else reject()
    case _ => reject()
  }
  def paramIs(name: String, value: String): Directive0 = paramIs(name, _ == value)

  def formFields: Directive1[Map[String, String]] = extract { ctx =>
    ctx.request.as[HttpForm].right.toOption.map {
      case fd: FormData => fd.fields.toMap
      case _ => Map.empty[String, String]
    }.getOrElse(Map.empty[String, String])
  }
  def formFieldIs(name: String, value: String): Directive0 = formField(name).flatMap { v =>
    if (v == value) pass else reject()
  }

  def queryParams: Directive1[Map[String, String]] = extract(_.request.uri.query.toMap)

  def allParams: Directive1[Map[String, String]] = queryParams.flatMap(qm => formFields.map(fm => fm ++ qm))

  def openIdParams: Directive1[Map[String, String]] =
    allParams.map(ps => ps.filter { case (k,v) => k startsWith "openid." })

  def isMode(mode: String): Directive0 = paramIs(Keys.mode.openid, mode)
  def isImmediate = isMode(Modes.checkid_immediate)
  def isSetup = isMode(Modes.checkid_setup)
  def isAssociate = isMode(Modes.associate)
  def isCheckAuth = isMode(Modes.check_authentication)


  def localIdOption: Directive1[Option[LocalId]] =
    paramOpt(Keys.identity.openid).flatMap {
      case Some(LocalIdParts(lid)) => provide(Some(lid))
      case _ => provide(None)
    }

  def localId: Directive1[LocalId] = localIdOption.flatMap {
    case Some(lid) => provide(lid)
    case _ => reject()
  }

  def extractRealm: Directive1[Ident] =
    (localId.map(_.realm) | provide(settings.defaultRealm)).flatMap {
      case realm if settings.acceptRealm(realm) => provide(realm)
      case _ => reject()
    }

  object LocalIdParts {
    def unapply(id: String): Option[LocalId] = {
      val segments = Uri(id).path.toString().split('/').filter(_.trim.nonEmpty).toList
      segments match {
        case realm :: name:: Nil => Some(LocalId(realm, name))
        case name :: Nil => Some(LocalId(settings.defaultRealm.name, name))
        case _ => None
      }
    }
  }

  def setupNeededResponse = Map(Keys.ns.openid -> openid20, Keys.mode.openid -> Modes.setup_needed)

  def userCancelResponse = Map(Keys.ns.openid -> openid20, Keys.mode.openid -> Modes.cancel)

  def errorResponse(direct: Boolean, msg: String) = {
    val c = settings.contact.map(s => Map(Keys.contact -> s)).getOrElse(Map.empty)
    val params: Map[String, String] = c ++ Map(
     Keys.ns -> openid20,
     Keys.mode -> Modes.error,
     Keys.error -> msg
    ) map  { case (k, v) => if (direct) (k.name, v) else (k.openid, v) }
    params ++ (if (direct) Map("iserror" -> "true") else Map.empty)
  }

  def signinCancel: Directive0 = paramIs("porter.submitType", "Cancel")
  def signinSubmit: Directive0 = paramIs("porter.submitType", "Sign in")
  def continueSubmit: Directive0 = paramIs("porter.submitType", "Continue")

  def nextNonce = {
    val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    df.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    df.format(new java.util.Date()) + java.util.UUID.randomUUID().toString.replace('-', 'z')
  }

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

  def sRegExtensionFields: Directive1[RequestedAttributes] =
    for {
      opt <- paramOpt(SRegKeys.optional.openid)
      req <- paramOpt(SRegKeys.required.openid)
      url <- paramOpt(SRegKeys.policy_url.openid)
    } yield {
      import porter.util._
      val optnames = opt.map(s => split(s, ',').filter(SRegAttributes.allNames.contains)) getOrElse Nil
      val reqnames = req.map(s => split(s, ',').filter(SRegAttributes.allNames.contains)) getOrElse Nil
      RequestedAttributes(
        optnames.map(s => Key("sreg."+s)),
        reqnames.map(s => Key("sreg."+s)), url)
    }

}

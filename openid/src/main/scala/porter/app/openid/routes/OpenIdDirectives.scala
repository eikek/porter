package porter.app.openid.routes

import spray.routing._
import spray.http._
import spray.routing.Directives._
import shapeless.HNil
import spray.httpx.marshalling.{ToResponseMarshallingContext, ToResponseMarshallable}
import spray.http.HttpResponse
import porter.model.Ident

trait OpenIdDirectives extends Provides {
  import _root_.porter.app.openid.common._
  import spray.httpx.unmarshalling._
  import shapeless.HList.ListCompat._

  implicit class KeyValueResponse(value: Map[String, String]) extends ToResponseMarshallable {
    def marshal(ctx: ToResponseMarshallingContext) = {
      val kvs = value.map { case (k, v) => s"$k:$v\n" }.mkString
      ctx.marshalTo(HttpResponse(entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, kvs)))
    }
  }

  def openIdEndpoint = PathMatchers.separateOnSlashes(settings.endpointUrl.path.dropChars(1).toString())

  def redirectToRelyingParty(params: Map[String, String]) = param(Keys.return_to.openid) { returnto =>
    redirect(Uri(returnto).withQuery(params), StatusCodes.Found)
  }

  def param(name: String): Directive1[String] = anyParam(name)

  def paramIs(name: String, pred: String => Boolean): Directive0 = param(name).hflatMap { v =>
    if (pred(v.head)) pass else reject()
  }
  def paramIs(name: String, value: String): Directive0 = paramIs(name, _ == value)

  def formFields: Directive1[Map[String, String]] = extract { ctx =>
    ctx.request.as[HttpForm].right.toOption.map {
      case fd: FormData => fd.fields.toMap
      case _ => Map.empty[String, String]
    }.getOrElse(Map.empty[String, String])
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
    param(Keys.identity.openid).hflatMap {
      case LocalIdParts(lid) #: HNil => provide(Some(lid))
      case _ => provide(None)
    }

  def localId: Directive1[LocalId] = localIdOption.hflatMap {
    case Some(lid) #: HNil => provide(lid)
    case _ => reject()
  }

  def extractRealm: Directive1[Ident] =
    (localId.map(_.realm) | provide(settings.defaultRealm)).hflatMap {
      case realm #: HNil if settings.acceptRealm(realm) =>
        provide(realm)
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

  def userCancel: Directive0 = paramIs("submitType", "Cancel")
  def userSubmit: Directive0 = paramIs("submitType", _ != "Cancel")
}

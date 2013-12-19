package porter.app.openid.routes

import porter.app.openid._
import porter.util.Base64
import porter.app.openid
import akka.util.Timeout
import scala.concurrent.Future
import spray.routing._
import spray.routing.Directives._
import shapeless.HNil
import porter.app.openid.AssocActor.GetToken
import porter.app.openid.AssocActor.DHOptions
import porter.app.openid.AssocActor.DHParams
import scala.Some
import porter.app.openid.AssocActor.CreatePrivateAssoc
import porter.app.openid.AssocActor.AssocToken
import scala.util.Success
import porter.app.openid.AssocActor.GetTokenResult
import porter.app.openid.AssocActor.CreateAssoc

trait AssociationDirectives extends OpenIdDirectives {
  import _root_.porter.app.openid.common._
  import akka.pattern.ask
  import shapeless.HList.ListCompat._

  private def assocLookupFuture(handle: String)(implicit timeout: Timeout) = {
    (assocActor ? GetToken(handle)).mapTo[GetTokenResult].map {
      case GetTokenResult(_, Some(token)) => Some(token)
      case _ => None
    }
  }
  private def createPrivateAssocFuture(implicit timeout: Timeout) =
    (assocActor ? CreatePrivateAssoc(AssocType.HmacSha256, SessionType.NoEncryption)).mapTo[GetTokenResult]

  def associationFuture(handle: Option[String])(implicit timeout: Timeout): Future[Association] = {
    lazy val createAssoc = createPrivateAssocFuture.flatMap {
      case GetTokenResult(h, Some(t)) => Future.successful(Association(h, t))
      case _ => Future.failed(new Exception("No assocation found and unable to create private one."))
    }
    val lookupAssoc = handle match {
      case Some(h) => assocLookupFuture(h).map {
        case Some(t) if t.isValid => Some(Association(h, t))
        case _ => None
      }
      case _ => Future.successful(None)
    }
    lookupAssoc.flatMap {
      case Some(a) => Future.successful(a)
      case _ => createAssoc
    }
  }
  def association(handle: Option[String])(implicit timeout: Timeout): Directive1[Association] =
    onSuccess(associationFuture(handle))

  def lookupAssociation(pred: AssocToken => Boolean)(implicit timeout: Timeout): Directive1[Association] =
    param(Keys.assoc_handle.openid).flatMap { handle =>
      onComplete(assocLookupFuture(handle)).flatMap {
        case Success(Some(token)) if pred(token) => provide(Association(handle, token))
        case _ => reject()
      }
    }

  private def extractAssociationReq: Directive1[CreateAssoc] = formFields.hflatMap {
    case CreateAssocExtr(req) #: HNil => provide(req)
    case _ => reject()
  }

  def createAssociation(implicit timeout: Timeout) = extractAssociationReq { req =>
    val f = (assocActor ? req).mapTo[GetTokenResult]
    onSuccess(f.map(_.toParameterMap)) { params =>
      complete(params)
    }
  }

  def checkAssocResponse(valid: Boolean, handle: String) = Map(
    Keys.ns.name -> openid20,
    Keys.is_valid.name -> valid.toString,
    Keys.invalidate_handle.name -> handle
  )

  implicit class GetTokenResponseMap(result: GetTokenResult) {
    def toParameterMap = {
      val standard = result.token match {
        case Some(AssocToken(at, st, mac, valid, priv, _)) if !priv =>
          Map(Keys.assoc_type.name -> at.name,
            Keys.ns.name -> openid20,
            Keys.assoc_handle.name -> result.handle,
            Keys.session_type.name -> st.name,
            Keys.expires_in.name -> valid.toSeconds.toString)
        case _ => Map.empty[String, String]
      }
      val crypt = result.token match {
        case Some(AssocToken(_, _, mac, _, _, None)) =>
          Map(Keys.mac_key.name -> Base64.encode(mac.getEncoded))
        case Some(AssocToken(_, _, _, _, _, Some(DHParams(macEnc, kpair)))) =>
          Map(Keys.enc_mac_key.name -> Base64.encode(macEnc),
            Keys.dh_server_public.name -> Base64.encode(kpair.getPublic.getY.toByteArray))
        case _ => Map.empty[String, String]
      }
      standard ++ crypt
    }
  }

  object CreateAssocExtr {
    def unapply(map: Map[String, String]): Option[CreateAssoc] = {
      lazy val noencr = for {
        at <- map.get(Keys.assoc_type.openid).flatMap(AssocType.apply)
        st <- map.get(Keys.session_type.openid).flatMap(SessionType.apply)
        if st == SessionType.NoEncryption
      } yield CreateAssoc(at, st, None)
      val enc = for {
        st <- map.get(Keys.session_type.openid).flatMap(SessionType.apply)
        if st == SessionType.DHSha1 || st == SessionType.DHSha256
        at <- map.get(Keys.assoc_type.openid).flatMap(AssocType.apply)
        dhmod <- map.get(Keys.dh_modulus.openid).orElse(Some(Crypt.DH.defaultModulusBase64))
        dhgen <- map.get(Keys.dh_gen.openid).orElse(Some(Crypt.DH.defaultG.toString()))
        dhpub <- map.get(Keys.dh_consumer_public.openid)
      } yield {
        val bdhmod = BigInt(Base64.decode(dhmod).toArray)
        val bdhgen = BigInt(dhgen)
        val bdhpub = BigInt(Base64.decode(dhpub).toArray)
        CreateAssoc(at, st, Some(DHOptions(bdhmod, bdhgen, bdhpub)))
      }
      enc.orElse(noencr)
    }
  }
}

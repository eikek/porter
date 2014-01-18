package porter.app.openid.routes

import porter.model.{PropertyList, Property, Account}
import spray.http._
import spray.httpx.marshalling.{ToResponseMarshallingContext, ToResponseMarshallable}
import spray.http.HttpResponse
import scala.Some
import porter.app.openid.AssocActor.{DHParams, AssocToken, GetTokenResult}
import porter.app.openid.common._
import porter.util.Base64
import java.util.{TimeZone, Locale}

object Implicits {

  implicit class KeyValueResponse(value: Map[String, String]) extends ToResponseMarshallable {
    def marshal(ctx: ToResponseMarshallingContext) = {
      val error = value.get("iserror").exists(_ == "true")
      val kvs = (value - "iserror").map { case (k, v) => s"$k:$v\n" }.mkString
      val status = if (error) StatusCodes.BadRequest else StatusCodes.OK
      ctx.marshalTo(HttpResponse(status = status, entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, kvs)))
    }
  }

  implicit class UriAdds(uri: Uri) {

    def matchesRealm(openIdRealm: String): Boolean = {
      val realm = Uri(openIdRealm)
      lazy val scheme = uri.scheme == realm.scheme
      lazy val port = uri.effectivePort == realm.effectivePort
      lazy val path = uri.path.startsWith(realm.path)
      lazy val nofragm = realm.fragment.isEmpty
      lazy val domain =
        if (realm.authority.host.address.contains("*.")) {
          val realmHost = realm.authority.host.address.substring(1).toLowerCase
          val uriHost = "."+uri.authority.host.address.toLowerCase
          uriHost.endsWith(realmHost)
        } else {
          realm.authority == uri.authority
        }
      scheme && port && path && nofragm && domain
    }

    def appendPath(path: String): Uri =
      uri.withPath(uri.path + path)
  }

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
}

package porter.app.openid

import scala.util.Try
import java.io.InputStream
import porter.model.{Account, Ident}
import porter.app.openid.AssocActor.AssocToken
import spray.http.Uri

package object common {

  type Supplier = () => Try[InputStream]

  case class Key(name: String) {
    val openid = s"openid.$name"
  }
  case class LocalId(realm: Ident, account: Ident)
  case class Authenticated(account: Account, association: Association)
  case class Association(handle: String, token: AssocToken)

  val openid20 = "http://specs.openid.net/auth/2.0"
  val identitySelect = "http://specs.openid.net/auth/2.0/identifier_select"

  trait OpenIdRequest {
    def isVersion2: Boolean
  }

  object Keys extends ObjectRegistry {
    type Elem = Key
    val ns = register(Key("ns"))
    val mode = register(Key("mode"))
    val error = register(Key("error"))
    val contact = register(Key("contact"))
    val reference = register(Key("reference"))
    val assoc_type = register(Key("assoc_type"))
    val session_type = register(Key("session_type"))
    val dh_modulus = register(Key("dh_modulus"))
    val dh_gen = register(Key("dh_gen"))
    val dh_consumer_public = register(Key("dh_consumer_public"))
    val assoc_handle = register(Key("assoc_handle"))
    val expires_in = register(Key("expires_in"))
    val mac_key = register(Key("mac_key"))
    val dh_server_public = register(Key("dh_server_public"))
    val enc_mac_key = register(Key("enc_mac_key"))
    val error_code = register(Key("error_code"))
    val claimed_id = register(Key("claimed_id"))
    val identity = register(Key("identity"))
    val return_to = register(Key("return_to"))
    val realm = register(Key("realm"))
    val op_endpoint = register(Key("op_endpoint"))
    val response_nonce = register(Key("response_nonce"))
    val invalidate_handle = register(Key("invalidate_handle"))
    val signed = register(Key("signed"))
    val sig = register(Key("sig"))
    val is_valid = register(Key("is_valid"))
  }

  object Modes extends ObjectRegistry {
    type Elem = String
    val error = register("error")
    val associate = register("associate")
    val checkid_immediate = register("checkid_immediate")
    val checkid_setup = register("checkid_setup")
    val id_res = register("id_res")
    val setup_needed = register("setup_needed")
    val cancel = register("cancel")
    val check_authentication = register("check_authentication")
  }

  implicit class RealmVerify(uri: Uri) {

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
  }
}

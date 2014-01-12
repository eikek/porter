package porter.app.openid

import scala.util.Try
import java.io.InputStream
import porter.model.{Account, Ident}
import porter.app.openid.AssocActor.AssocToken
import spray.http.Uri
import porter.util.ObjectRegistry
import porter.app.openid.common.MustacheContext.KeyedData

package object common {

  type Supplier = () => Try[InputStream]

  case class Key(name: String) extends KeyedData(name) {
    val openid = s"openid.$name"
  }
  case class LocalId(realm: Ident, account: Ident)
  case class Authenticated(account: Account, association: Association)
  case class Association(handle: String, token: AssocToken)

  val openid20 = "http://specs.openid.net/auth/2.0"
  val sreg10Ns = "http://openid.net/sreg/1.0"
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

  object SRegKeys extends ObjectRegistry {
    type Elem = Key
    val required = register(Key("sreg.required"))
    val optional = register(Key("sreg.optional"))
    val policy_url = register(Key("sreg.policy_url"))
  }
  object SRegAttributes extends ObjectRegistry {
    type Elem = Key
    val ns = register(Key("ns.sreg"))
    val nickname = register(Key("sreg.nickname"))
    val email = register(Key("sreg.email"))
    val fullname = register(Key("sreg.fullname"))
    val dob = register(Key("sreg.dob"))
    val gender = register(Key("sreg.gender"))
    val postcode = register(Key("sreg.postcode"))
    val country = register(Key("sreg.country"))
    val language = register(Key("sreg.language"))
    val timezone = register(Key("sreg.timezone"))

    lazy val allNames = all.map(_.name.substring("sreg.".length))
  }

  case class RequestedAttributes(optional: List[Key], required: List[Key], url: Option[String]) {
    def isEmpty = optional.isEmpty && required.isEmpty
    def nonEmpty = !isEmpty
  }
}

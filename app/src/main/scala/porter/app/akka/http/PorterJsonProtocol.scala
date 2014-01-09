package porter.app.akka.http

import porter.model._
import porter.app.akka.Porter.Messages.authz._
import porter.client.json.MessageJsonProtocol

object PorterJsonProtocol extends MessageJsonProtocol {

  implicit val getPolicyFormat = jsonFormat2(GetPolicy)
  case class JsPolicyResp(account: Ident, rules: List[String])
  implicit val jsPolicyRespFormat = jsonFormat2(JsPolicyResp)

}

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

package porter.app.akka.http

import porter.model._
import porter.app.akka.Porter.Messages.authz._
import porter.client.json.MessageJsonProtocol

object PorterJsonProtocol extends MessageJsonProtocol {

  implicit val getPolicyFormat = jsonFormat2(GetPolicy)
  case class JsPolicyResp(account: Ident, rules: List[String])
  implicit val jsPolicyRespFormat = jsonFormat2(JsPolicyResp)

}

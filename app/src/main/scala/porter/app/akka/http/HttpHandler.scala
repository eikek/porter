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

import akka.actor._
import spray.can.Http
import akka.actor.Terminated
import porter.auth.Decider
import porter.model.PasswordCrypt
import porter.app.akka.PorterRef

class HttpHandler(porter: PorterRef, decider: Decider) extends Actor with ActorLogging {
  import HttpHandler._

  var connCreated = 0
  var connections = 0

  def receive = {
    case Http.Bound(addr) =>
      log.info("Bound http interface to "+ addr)

    case Http.Connected(_, _) =>
      val newchild = context.watch(context.actorOf(HttpConnection(porter, decider), name = s"httpconn$connCreated"))
      connections += 1; connCreated += 1
      sender ! Http.Register(newchild)

    case Terminated(ref) =>
      connections -= 1
      log.debug(s"Actor $ref terminated. There are currently $connections http connections left.")

    case GetConnCount =>
      sender ! connections
  }

}

object HttpHandler {

  case object GetConnCount extends Serializable

  def apply(porter: PorterRef, decider: Decider) =
    Props(classOf[HttpHandler], porter, decider)

}
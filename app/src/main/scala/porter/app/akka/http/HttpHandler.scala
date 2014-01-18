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

class HttpHandler(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) extends Actor with ActorLogging {
  import HttpHandler._

  var connections = 0

  def receive = {
    case Http.Bound(addr) =>
      log.info("Bound http interface to "+ addr)

    case Http.Connected(_, _) =>
      val newchild = context.watch(context.actorOf(HttpConnection(porter, decider, crypt), name = s"httpconn$connections"))
      connections += 1
      logConnections()
      sender ! Http.Register(newchild)

    case Terminated(_) =>
      connections -= 1
      logConnections()

    case GetConnCount =>
      sender ! connections
  }

  def logConnections() {
    log.info(s"There are currently $connections http connections.")
  }
}

object HttpHandler {

  case object GetConnCount extends Serializable

  def apply(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) =
    Props(classOf[HttpHandler], porter, decider, crypt)

}
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
import akka.util.Timeout
import spray.routing._
import porter.auth.Decider
import akka.io.Tcp.ConnectionClosed
import porter.model.PasswordCrypt

class HttpConnection(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) extends HttpServiceActor with ActorLogging {
  implicit val timeout = Timeout(5000)
  import context.dispatcher

  private val authRoute = AuthService(porter, decider).route
  private val storeRoute = StoreService(porter, decider, crypt).route

  def receive = runRoute {
    authRoute ~ storeRoute
  }

  override def onConnectionClosed(ev: ConnectionClosed) = context.stop(self)
}

object HttpConnection {
  def apply(porter: ActorRef, decider: Decider, crypt: PasswordCrypt) = Props(classOf[HttpConnection], porter, decider, crypt)
}
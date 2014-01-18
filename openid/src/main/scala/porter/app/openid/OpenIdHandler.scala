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

package porter.app.openid

import akka.actor._
import spray.can.Http
import akka.actor.Terminated
import scala.concurrent.{Future, ExecutionContext}
import akka.util.Timeout
import akka.io.{IO, Tcp}
import porter.app.openid.CacheDirActor.CacheDirOpts

class OpenIdHandler(porter: ActorRef, settings: OpenIdServiceSettings) extends Actor with ActorLogging {
  import OpenIdHandler._
  var connections = 0

  val assocActor = context.actorOf(AssocActor())
  val avatarActor = context.actorOf(AvatarActor(porter, settings.avatarCacheDir.map(dir => CacheDirOpts(dir, settings.avatarCacheDirSize))))
  val serviceProps = OpenIdService(porter, assocActor, avatarActor, settings)

  def receive = {
    case Http.Bound(addr) =>
      log.info("Bound http interface to "+ addr)

    case Http.Connected(_, _) =>
      val newchild = context.watch(context.actorOf(serviceProps, name = s"openidconn$connections"))
      connections += 1
      sender ! Http.Register(newchild)

    case Terminated(_) =>
      connections -= 1

    case GetConnCount =>
      sender ! connections
  }
}

object OpenIdHandler {
  case object GetConnCount

  def apply(porter: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdHandler], porter, settings)

  /**
   * Binds the http service to the given address. The Future is completed
   * with either [[akka.io.Tcp.Bound]] or [[akka.io.Tcp.CommandFailed]].
   *
   * @param host
   * @param port
   * @param system
   * @param ec
   * @return
   */
  def bind(porter: ActorRef, settings: OpenIdServiceSettings, host: String, port: Int)
          (implicit system: ActorSystem, ec: ExecutionContext, bindTimeout: Timeout): Future[Tcp.Event] = {
    import akka.pattern.ask
    val httpHandler = system.actorOf(OpenIdHandler(porter, settings), name = "porter-openid")
    (IO(Http) ? Http.Bind(httpHandler, interface = host, port = port)).mapTo[Tcp.Event]
  }

}

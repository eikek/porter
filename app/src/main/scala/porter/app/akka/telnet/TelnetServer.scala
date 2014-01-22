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

package porter.app.akka.telnet

import akka.actor._
import akka.io.{IO, Tcp}
import porter.app.akka.telnet.TelnetServer.GetConnCount
import akka.actor.Terminated
import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext
import akka.io.Tcp.Event
import akka.util.Timeout

class TelnetServer(porter: ActorRef) extends Actor with ActorLogging {

  private var connCreated = 0
  private var connections = 0

  def receive = {
    case Tcp.Connected(_, _) =>
      val tcpConnection = sender
      val newchild = context.watch(context.actorOf(TelnetConnection.props(porter, tcpConnection), name = s"telnetconn$connCreated"))
      connections += 1; connCreated += 1
      sender ! Tcp.Register(newchild)

    case Terminated(ref) =>
      connections -= 1
      log.debug(s"Actor $ref terminated. There are currently $connections telnet connections.")

    case GetConnCount =>
      sender ! connections
  }
}
object TelnetServer {

  case object GetConnCount extends Serializable

  def apply(porter: ActorRef) = Props(classOf[TelnetServer], porter)

  /**
   * Binds the telnet service to the given address. The Future is completed
   * with either [[akka.io.Tcp.Bound]] or [[akka.io.Tcp.CommandFailed]].
   *
   * @param host
   * @param port
   * @param system
   * @param ec
   * @return
   */
  def bind(porter: ActorRef, host: String, port: Int)
          (implicit system: ActorSystem, ec: ExecutionContext, bindTimeout: Timeout) = {
    import akka.pattern.ask
    val telnetEndpoint = new InetSocketAddress(host, port)
    val telnetServer = system.actorOf(apply(porter), name = "porter-telnet")
    (IO(Tcp) ? Tcp.Bind(telnetServer, telnetEndpoint)).mapTo[Event]
  }
}
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

package porter.dist

import akka.actor._
import porter.app.akka.Porter
import porter.app.akka.telnet.TelnetServer
import porter.app.akka.http.HttpHandler
import porter.app.openid.{OpenIdHandler, OpenIdSettings}
import akka.util.Timeout
import akka.io.{Tcp, IO}
import java.net.InetSocketAddress
import porter.dist.MainActor._
import spray.can.Http
import porter.dist.MainActor.BindOpenid
import porter.dist.MainActor.BindHttp
import porter.dist.MainActor.ServiceBound
import porter.dist.MainActor.ServiceBoundFailure
import porter.dist.MainActor.BindTelnet
import scala.concurrent.Future

class MainActor extends Actor with ActorLogging {
  import context.dispatcher
  import akka.pattern.ask
  import akka.pattern.pipe
  implicit val timeout = Timeout(3000)
  implicit val system = context.system

  val main = MainExt(context.system)
  val openidSettings = OpenIdSettings(context.system)

  val porter = Porter(context.system).fromSubConfig(context)
  if (main.isRemote) {
    println(s"\n---\n--- Porter remote actor listening on ${main.pathFor(porter)} \n---")
  }
  val telnetService = context.actorOf(TelnetServer(porter), "telnet")
  val httpService = context.actorOf(HttpHandler(porter, main.http.decider), "http")
  val openIdService = context.actorOf(OpenIdHandler(porter, openidSettings), "openid")

  private def toTcp(actor: ActorRef, host: String, port: Int) = {
    val endpoint = new InetSocketAddress(host, port)
    IO(Tcp) ? Tcp.Bind(actor, endpoint)
  }
  private def toHttp(actor: ActorRef, host: String, port: Int) = {
    IO(Http) ? Http.Bind(actor, interface = host, port = port)
  }

  private def bind(name: String, bind: => Future[Any]) {
    def mapBindResponse(f: Future[Any]): Future[Any] = {
      f.map({
        case ok:Tcp.Bound => ServiceBound(name, ok)
        case f: Tcp.CommandFailed => ServiceBoundFailure(name, new Exception(f.cmd.toString))
        case f@_ => ServiceBoundFailure(name, new Exception(s"Unexpected response to bind: $f"))
      }).recover({case x => ServiceBoundFailure(name, x) })
    }

    mapBindResponse(bind) pipeTo sender
  }
  
  def receive = {
    case BindTelnet(host, port) =>
      bind("telnet", toTcp(telnetService, host, port))

    case BindHttp(host, port) =>
      bind("http", toHttp(httpService, host, port))

    case BindOpenid(host, port) =>
      bind("openid", toHttp(openIdService, host, port))

    case b@ServiceBound(name, addr) =>
      println(s"--- Bound $name to ${addr.localAddress}")

    case ServiceBoundFailure(name, ex) =>
      log.error(ex, s"Binding $name failed")
      system.shutdown()
  }


  override def preStart() = {
    def bindService(bind: ServiceBind) {
      (self ? bind).mapTo[ServiceBound] pipeTo self
    }
    
    if (main.telnet.enabled) {
      bindService(BindTelnet(main.telnet.host, main.telnet.port))
    }
    if (main.http.iface.enabled) {
      bindService(BindHttp(main.http.iface.host, main.http.iface.port))
    }
    if (main.openid.enabled) {
      bindService(BindOpenid(main.openid.host, main.openid.port))
    }
  }
}

object MainActor {

  def apply() = Props(classOf[MainActor])

  sealed trait ServiceBind {
    def host: String
    def port: Int
  }
  case class BindHttp(host: String, port: Int) extends ServiceBind
  case class BindTelnet(host: String, port: Int) extends ServiceBind
  case class BindOpenid(host: String, port: Int) extends ServiceBind
  case class ServiceBound(name: String, bount: Tcp.Bound)
  case class ServiceBoundFailure(name: String, ex: Throwable)
}

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

import akka.actor.{ActorLogging, Actor, Props, ActorRef}
import spray.routing.HttpServiceActor
import porter.app.openid.routes._
import akka.util.Timeout
import spray.http._
import akka.io.Tcp.ConnectionClosed
import porter.app.openid.routes.manage.ManageRoutes
import org.eknet.spray.openid.provider._
import porter.app.openid.common.LocalId
import porter.model.Account
import porter.model.Account
import org.eknet.spray.openid.model.CheckIdRequest
import porter.model

class OpenIdService(val porterRef: ActorRef, val assocActor: ActorRef, val avatarRef: ActorRef, val settings: OpenIdServiceSettings) extends HttpServiceActor
  with ActorLogging
  with OpenIdActors
  with OpenIdProviderHook
  with StaticRoute
  with ManageRoutes
  with AvatarRoute {

  implicit def dispatcher = context.dispatcher
  implicit val timeout = Timeout(3000)
  implicit val refFactory = context

  private val discoverySetting = DiscoverySettings.forPathPrefix(settings.endpointBaseUrl)
  private val openId = new ProviderRoute(EndpointSettings(createHook, assocActor), discoverySetting)

  def receive = runRoute {
    homeRoute ~ avatarRoute ~ staticRoute ~ openId.route
  }

  override def onConnectionClosed(ev: ConnectionClosed) = context.stop(self)

  object LocalIdParts {
    def unapply(id: String): Option[LocalId] = {
      val segments = Uri(id).path.toString().split('/').filter(_.trim.nonEmpty).toList
      segments match {
        case realm :: name:: Nil => Some(LocalId(realm, name))
        case name :: Nil => Some(LocalId(settings.defaultRealm.name, name))
        case _ => None
      }
    }
  }
}

object OpenIdService {

  def apply(porter: ActorRef, assocActor: ActorRef, avatarActor: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdService], porter, assocActor, avatarActor, settings)

  def apply(porter: ActorRef, avatarActor: ActorRef, settings: OpenIdServiceSettings) =
    Props(classOf[OpenIdActor], porter, avatarActor, settings)

  class OpenIdActor(porter: ActorRef, settings: OpenIdServiceSettings) extends Actor {
    val assocActor = context.actorOf(AssociationActor(), "association")
    val openid = context.actorOf(OpenIdService(porter, assocActor, settings), "service")
    def receive = {
      case m => openid.forward(m)
    }
  }
}

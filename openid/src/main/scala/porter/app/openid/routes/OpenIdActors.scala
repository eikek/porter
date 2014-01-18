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

package porter.app.openid.routes

import porter.app.openid.OpenIdServiceSettings
import scala.concurrent.ExecutionContext
import akka.actor.{ActorRefFactory, ActorSystem, ActorRef}
import akka.util.Timeout
import akka.event.LoggingAdapter

trait OpenIdActors {

  def log: LoggingAdapter
  def settings: OpenIdServiceSettings

  implicit def refFactory: ActorRefFactory
  implicit def dispatcher: ExecutionContext
  implicit def timeout: Timeout

  def assocActor: ActorRef
  def porterRef: ActorRef
  def avatarRef: ActorRef
}

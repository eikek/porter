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

package porter.app.akka

import akka.actor.{ActorSelection, Actor, ActorContext, ActorRef}
import akka.util.Timeout
import scala.concurrent.Future
import akka.pattern.{AskableActorRef, AskableActorSelection}

sealed abstract class PorterRef {

  def forward(message: Any)(implicit context: ActorContext)
  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit
  def ?(message: Any)(implicit timeout: Timeout): Future[Any]
  def unwrap: Either[ActorRef, ActorSelection]

}

object PorterRef {
  import language.implicitConversions

  implicit def apply(selection: ActorSelection): PorterRef = new FromSelection(selection)
  implicit def apply(ref: ActorRef): PorterRef = new FromRef(ref)

  private final class FromRef(ref: ActorRef) extends PorterRef {
    private val askable = new AskableActorRef(ref)
    def !(message: Any)(implicit sender: ActorRef) = ref.tell(message, sender)
    def forward(message: Any)(implicit context: ActorContext) = ref.forward(message)(context)
    def ?(message: Any)(implicit timeout: Timeout) = askable ? message
    def unwrap = Left(ref)
  }

  private final class FromSelection(selection: ActorSelection) extends PorterRef {
    private val askable = new AskableActorSelection(selection)
    def !(message: Any)(implicit sender: ActorRef) = selection.tell(message, sender)
    def forward(message: Any)(implicit context: ActorContext) = selection.tell(message, context.sender)
    def ?(message: Any)(implicit timeout: Timeout) = askable ? message
    def unwrap = Right(selection)
  }
}

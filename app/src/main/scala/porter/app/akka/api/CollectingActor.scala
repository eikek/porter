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

package porter.app.akka.api

import akka.actor.{Status, ActorLogging, Actor, ActorRef}

/**
 * Sends the `req` to all `target` actors and collects the resulting replies. The replies
 * are merged into one and send back to `client`
 *
 */
private[api] abstract class CollectingActor(client: ActorRef, req: PorterMessage, targets: Iterable[ActorRef]) extends Actor with ActorLogging {

  import scala.language.reflectiveCalls
  type Res

  def empty: Res

  val Extr: {
    def unapply(a: Any): Option[Res]
  }

  def merge(r1: Res, r2: Res): Res

  def receive = waitfor(targets.toSet, empty)

  override final def preStart() = {
    targets foreach (_ ! req)
  }

  def waitfor(refs: Set[ActorRef], res: Res): Receive = {
    case Extr(resp) if refs.size == 1 =>
      client ! merge(resp, res)
      context.stop(self)

    case Extr(resp) if refs.size > 1 =>
      context.become(waitfor(refs - sender, merge(resp, res)))

    case m@Status.Failure(x) =>
      log.error(x, "Error in response")
      client ! m

    case m@_ =>
      log.warning("Unknown message: "+ m)
      client ! Unknown
  }
}

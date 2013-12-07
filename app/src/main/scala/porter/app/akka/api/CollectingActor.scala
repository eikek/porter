package porter.app.akka.api

import akka.actor.{Status, ActorLogging, Actor, ActorRef}

/**
 * Sends the `req` to all `target` actors and collects the resulting replies. The replies
 * are merged into one and send back to `client`
 *
 * @since 05.12.13 20:13
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

package porter.app.client.spray

import akka.actor.ActorRef
import porter.auth.{OneSuccessfulVote, Decider}
import porter.model.Ident

case class PorterContext(porterRef: ActorRef, realm: Ident, decider: Decider = OneSuccessfulVote)

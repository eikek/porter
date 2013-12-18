package porter.app.openid.routes

import porter.app.openid.OpenIdServiceSettings
import scala.concurrent.ExecutionContext
import akka.actor.{ActorSystem, ActorRef}
import akka.util.Timeout

trait Provides {

  def settings: OpenIdServiceSettings

  implicit def system: ActorSystem
  implicit def dispatcher: ExecutionContext
  implicit def timeout: Timeout

  def assocActor: ActorRef
  def porter: ActorRef
}

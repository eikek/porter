package porter.app.akka

import akka.actor.{PoisonPill, Props, ActorSystem}
import scala.concurrent.Await
import akka.util.Timeout
import porter.model.{Realm, Ident, Credentials, PasswordCredentials}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 16:44
 */
object Play extends App {
  import PorterActor._
  val system = ActorSystem("testing")

  val porterActor = system.actorOf(Props(classOf[PorterActor], new ConfiguredPorter(system.settings.config)))

  import akka.pattern.ask
  import scala.concurrent.duration._
  implicit val timeout = Timeout(10.seconds)

  try {
    val f = porterActor ? UpdateRealm(Realm(Ident.randomIdent, ""))
    val resp = Await.result(f, 10.seconds)
    println("\n"+ resp +"\n")
  } finally {
    porterActor ! PoisonPill
    Thread.sleep(500)
    system.shutdown()
  }

}

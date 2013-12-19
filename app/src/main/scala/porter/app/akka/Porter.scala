package porter.app.akka

import akka.actor._
import porter.app.akka.api._
import porter.app.PorterSettings

class Porter(system: ExtendedActorSystem) extends Extension {

  /**
   * Creates an actor path to a remote porter actor.
   *
   * @param host the host
   * @param port and port of the other akka system
   * @return
   */
  def remotePath(host: String, port: Int): ActorPath =
    ActorPath.fromString(s"akka.tcp://porter@$host:$port/user/porter/api")

  /**
   * Selects the remote porter actor at the given location. If an [[akka.actor.ActorRef]]
   * is needed, call `resolveOne()` on the returned selection.
   *
   * @param host host and
   * @param port port of the other actor system
   * @return
   */
  def select(host: String = "localhost", port: Int = 4554): ActorSelection =
    system.actorSelection(remotePath(host, port))


  /**
   * Creates a new porter actor within this actor system and returns the ref.
   *
   * @param settings settings to configure porter
   * @param name the name of the new actor
   * @return
   */
  def createPorter(factory: ActorRefFactory, settings: PorterSettings, name: String): ActorRef =
    factory.actorOf(PorterMain(settings), name)

  /**
   * Creates a new porter actor within this actor system and returns the ref. The
   * porter actor is created by looking up a [[com.typesafe.config.Config]] using
   * the given `configName` and using this to create a [[porter.app.PorterSettings]]
   * object.
   *
   * @param configName a name that is used to lookup a config via
   *                   `system.settings.config.getConfig` which is in turn used to
   *                   create a [[porter.app.PorterSettings]] object
   * @param actorName the name of the new actor
   * @return
   */
  def createPorter(factory: ActorRefFactory, configName: String, actorName: String = "api"): ActorRef = {
    val cfg = system.settings.config.getConfig(configName)
    val settings = PorterSettings.fromConfig(cfg, system.dynamicAccess)
    createPorter(factory, settings, actorName)
  }

  /**
   * For convenience, this creates a new porter actor within this actor system by
   * looking up a configuration of name "porter" and the actor name "porter-api".
   */
  lazy val main = createPorter(system, "porter")
}

object Porter extends ExtensionId[Porter] with ExtensionIdProvider {
  def lookup() = Porter

  def createExtension(system: ExtendedActorSystem) = new Porter(system)
  
  object Messages {
    val rules = RuleFactoryActor.messages
    val store = StoreActor.messages
    val mutableStore = MutableStoreActor.messages
    val authc = AuthcWorker.messages
    val authz = PolicyActor.messages
  }
}

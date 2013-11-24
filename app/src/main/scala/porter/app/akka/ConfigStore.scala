package porter.app.akka

import porter.store.{SimpleStore, Store}
import com.typesafe.config.Config
import porter.model.{Credentials, Ident}

/**
 * @author Eike Kettner eike.kettner@gmail.com
 * @since 23.11.13 18:51
 */
class ConfigStore(cfg: Config) extends SimpleStore {
  def realms = ???

  def groups = ???

  def accounts = ???
}

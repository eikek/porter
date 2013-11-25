package porter.app

import com.typesafe.config.Config
import porter.store.PropertiesStore
import porter.util.Properties
import scala.util.Try

class PropertyFileStore(cfg: Config) extends PropertiesStore(PropertyFileStore.fromConfig(cfg).get)

object PropertyFileStore {
  def fromConfig(cfg: Config): Try[Map[String, String]] = {
    val filename = cfg.getString("file")
    Properties.fromFile(new java.io.File(filename)).map(Properties.toMap)
  }
}

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

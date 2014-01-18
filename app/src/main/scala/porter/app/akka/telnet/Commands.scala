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

package porter.app.akka.telnet

import scala.concurrent.ExecutionContext
import akka.util.Timeout

trait Commands {

  def make(implicit executor: ExecutionContext, to: Timeout): Seq[Command]

  def makeDoc: String

  final def ++ (other: Commands) = {
    val self = this
    new Commands {
      def make(implicit executor: ExecutionContext, to: Timeout) = self.make ++ other.make
      def makeDoc = self.makeDoc + other.makeDoc
    }
  }

  final def reduce(implicit executor: ExecutionContext, to: Timeout): Command =
    (make :+ notFound) reduce (_ orElse _)

  private def notFound: Command = {
    case in@Input(msg, _, _, _) => in << s"Command '$msg' not found"
  }

  protected def propsToString(props: porter.model.Properties, start: String, sep: String, end: String): String = {
    def cutString(str: String) = if (str.length > 80) str.substring(0, 77)+"..." else str
    props.map({case (k,v) => k +"="+ cutString(v) }).mkString(start, sep, end)
  }
}
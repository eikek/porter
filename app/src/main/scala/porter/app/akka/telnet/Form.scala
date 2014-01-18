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

import scala.util.{Success, Try}
import java.util.UUID

abstract class Form extends Command {
  private val randPrefix = UUID.randomUUID().toString
  private lazy val prefixedFields = fields.map(f => randPrefix + f)

  def fields: List[String]

  def show: PartialFunction[Input, Boolean]

  def validateConvert: PartialFunction[(String, String), Try[Any]]

  def onComplete(in: Input): Unit

  final def apply(v1: Input) = chain(v1)
  final def isDefinedAt(x: Input) = chain.isDefinedAt(x)

  private def chain: Command = {
    case in if show.isDefinedAt(in) =>
      if (show(in)) {
        in.token = prefixedFields.head
        in.conn ! tcp(fields.head+": ")
      }

    case in if in.token.exists(prefixedFields.contains) =>
      findNext(in) match {
        case None => close(in)
        case Some(key) =>
          val value = validateConvert.lift(key -> in.msg.trim).getOrElse(Success(in.msg.trim))
          value.map { any =>
            in.session.add(key, any)
            findNext(in) match {
              case None => close(in)
              case Some(nkey) =>
                in.conn ! tcp(s"$nkey: ")
            }
          } recover { case x =>
            in.conn ! tcp(s"Error: ${x.getMessage}\n$key ")
          }
      }
  }

  private def findNext(in: Input) = fields.find(key => in.session.get(key).isEmpty)

  private def close(in: Input) {
    onComplete(in)
    in.session.remove(fields: _*)
    in.session.removeToken()
  }

}

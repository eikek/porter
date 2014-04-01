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

package porter.app.openid.routes

import porter.model.Account
import spray.routing.Route
import porter.app.client.PorterContext

package object manage {

  case class Action(name: String, pctx: PorterContext, account: Account)
  case class Message(level: String, text: String, items: List[String] = Nil) {
    def isEmpty = text.isEmpty && items.isEmpty
  }
  object Message {
    val empty = Message("", "")
    def info(text: String, items: List[String] = Nil) = Message("info", text, items)
    def error(text: String, items: List[String] = Nil) = Message("danger", text, items)
    def success(text: String, items: List[String] = Nil) = Message("success", text, items)
  }
  type Submission = PartialFunction[Action, Route]
}

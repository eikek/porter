package porter.app.openid.routes

import porter.model.Account
import spray.routing.Route
import porter.app.client.spray.PorterContext

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

package porter.app.openid.common

import scala.util.Try
import porter.app.openid._

object HtmlTemplates {
  import scala.xml._

  val loginTemplate = "login-template.html"
  val continueTemplate = "continue-template.html"
  val errorTemplate = "error-template.html"

  def createLoginPage(form: Node, info: Node)(implicit settings: OpenIdServiceSettings): Option[Try[String]] = {
    def loader(s: Supplier) = createPage(s, {
      case Elem(a, b, Attribute(x, Seq(y), z), d) if x == "id" && y.text == "porter.loginform" =>
        form
      case Elem(a, b, Attribute(x, Seq(y), z), d) if x == "id" && y.text == "porter.logininfo" =>
        info
    })
    settings.loadTemplate(loginTemplate).map(loader)
  }

  def createContinueTemplate(form: Node, info: Node)(implicit settings: OpenIdServiceSettings): Option[Try[String]] = {
    def loader(s: Supplier) = createPage(s, {
      case Elem(a, b, Attribute(x, Seq(y), z), d) if x == "id" && y.text == "porter.continueform" =>
        form
      case Elem(a, b, Attribute(x, Seq(y), z), d) if x == "id" && y.text == "porter.continueinfo" =>
        info
    })
    settings.loadTemplate(continueTemplate).map(loader)
  }

  def createErrorTemplate(implicit settings: OpenIdServiceSettings) =
    settings.loadTemplate(errorTemplate).map(s => createPage(s, PartialFunction.empty))
  
  def createPage(template: Supplier, trans: PartialFunction[Node, Node]): Try[String] = {
    val templ = template().map(XML.load)
    templ.map(transform(_, trans)).map(_.toString())
  }

  def transform(node: Node, trans: PartialFunction[Node, Node]): Node = {
    if (trans.isDefinedAt(node)) trans(node)
    else node match {
      case e: Elem =>
      if (e.descendant.exists(trans.isDefinedAt))
        e.copy(e.prefix, e.label, e.attributes, e.scope, e.minimizeEmpty,
          e.child.map(transform(_, trans)))
      else e
    case _ => node
    }
  }
}

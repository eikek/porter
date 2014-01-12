package porter.app.openid.common

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.util.{Failure, Success}

class MustacheTest extends FunSuite with ShouldMatchers {

  import Mustache._
  import Mustache.TemplateParser._

  test("parse to token stream") {
    val templ = " Hi, I'm {{name}}, so far {{#list}} its {{item}} time {{/list}}."
    val tokens = TemplateParser.parse(templ)
    val expect = List(StringToken(" Hi, I'm "),
      LookupToken("name"), StringToken(", so far "),
      BlockStart("list", false), StringToken(" its "),
      LookupToken("item"), StringToken(" time "),
      BlockEnd("list"), StringToken(".")
    )
    tokens.size should be (expect.size)
    tokens.zip(expect).foreach { case (t, e) =>
      assert(t === e)
    }
  }

  test("makeTemplate") {
    val templ = " Hi, I'm {{name}}, so far{{#list}} its {{item}} time{{/list}}."
    val template = Mustache(templ)
    val context = Map("name" -> "Eike", "list" -> Seq(Map("item" -> "1st"), Map("item"->"2nd")))
    template(context) should be (" Hi, I'm Eike, so far its 1st time its 2nd time.")

    val template2 = Mustache("{{#musketeers}}\n* {{.}}\n{{/musketeers}}")
    val out2 = template2(Map("musketeers" -> List("Athos", "Aramis", "Porthos")))
    out2 should be ("\n* Athos\n\n* Aramis\n\n* Porthos\n")
  }

  test("erroneous templates") {
    val Failure(e) = Mustache.tryApply("{{#list}} {{.}} as")
  }

  test("provided templates") {
    Mustache(getClass.getResourceAsStream("/porter/app/openid/assets/login-template.mustache"))
    Mustache(getClass.getResourceAsStream("/porter/app/openid/assets/continue-template.mustache"))
    Mustache(getClass.getResourceAsStream("/porter/app/openid/assets/error-template.mustache"))
  }

  test("inverse blocks") {
    val template = Mustache("Hallo, {{^name}}Unknown{{/name}}{{#name}}{{name}}{{/name}}.")
    val s1 = template(Map("test" -> "other"))
    s1 should be ("Hallo, Unknown.")
    val s2 = template(Map("name" -> "John"))
    s2 should be ("Hallo, John.")
    val s3 = template(Map("name" -> List()))
    s3 should be ("Hallo, Unknown.")
  }

  test("empty blocks") {
    val template = Mustache("Hello {{#flag}}Sir {{/flag}}Fauntleroy")
    val s1 = template(Map("flag" -> false))
    s1 should be ("Hello Fauntleroy")

    val s2 = template(Map("flag" -> true))
    s2 should be ("Hello Sir Fauntleroy")
  }
}

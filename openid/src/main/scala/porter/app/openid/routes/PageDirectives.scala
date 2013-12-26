package porter.app.openid.routes

import porter.app.openid.common.Keys
import spray.routing.Directives._
import spray.http._
import spray.http.HttpResponse
import porter.app.openid.common.LocalId
import porter.BuildInfo

trait PageDirectives {
  self: OpenIdDirectives =>

  import PageDirectives._

  private implicit class MapAdds[A](opt: Option[A]) {
    def toMap(f: A => Map[String, Any]) = opt match {
      case Some(a) => f(a)
      case _ => Map.empty[String, Any]
    }
  }

  private def loginPage(params: Map[String, String], lid: Option[LocalId], failed: Boolean) = {
    val context = defaultContext ++ Map(
      "realm" -> params.get(Keys.realm.openid).getOrElse(""),
      "identity" -> params.get(Keys.identity.openid).getOrElse(""),
      "endpointUrl" -> settings.endpointUrl.toString(),
      "params" -> params.map(t => Map("name" -> t._1, "value"->t._2)),
      "loginFailed" -> failed
    ) ++ lid.toMap(id => Map("localId" -> Map("realm" -> id.realm.name, "account" -> id.account.name)))

    settings.loginTemplate(context)
  }


  private def continueForm(returnto: Uri, params: Map[String, String]) = {
    val rto = returnto.copy(query = Uri.Query.Empty)
    val context = defaultContext ++ Map(
      "realm" -> params.get(Keys.realm.openid).getOrElse(""),
      "identity" -> params.get(Keys.identity.openid).getOrElse(""),
      "returnToUrl" -> rto,
      "params" -> (params ++ returnto.query.toMap).map(t => Map("name" -> t._1, "value"->t._2))
    )
    settings.continueTemplate(context)
  }

  private def errorPage(params: Map[String, String]) = {
    val context = defaultContext ++ Map("params" -> params.map(t => Map("name" -> t._1, "value"->t._2)))
    settings.errorTemplate(context)
  }

  def renderLoginPage(failed: Boolean) = allParams { req =>
    localIdOption { lidopt =>
      complete(HttpResponse(
        entity = HttpEntity(ContentType(MediaTypes.`text/html`),
          loginPage(req, lidopt, failed))))
    }
  }

  def renderErrorPage = allParams { params =>
    complete(HttpResponse(
      status = StatusCodes.BadRequest,
      entity = HttpEntity(ContentType(MediaTypes.`text/html`), errorPage(params))))
  }

  def renderContinuePage(params: Map[String, String]) = returnToUrl { uri =>
    complete(HttpResponse(
      entity = HttpEntity(ContentType(MediaTypes.`text/html`), continueForm(uri, params))))
  } ~ renderErrorPage

}

object PageDirectives {
  val defaultContext = Map(
    "porter" -> Map("version" -> BuildInfo.version,
      "revision" -> BuildInfo.revision,
      "builtTime" -> new java.util.Date(BuildInfo.buildTime))
  )

}
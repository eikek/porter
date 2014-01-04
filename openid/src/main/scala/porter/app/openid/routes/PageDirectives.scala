package porter.app.openid.routes

import porter.app.openid.common.Keys
import spray.routing.Directives._
import spray.http._
import spray.http.HttpResponse
import porter.app.openid.common.LocalId
import porter.BuildInfo

trait PageDirectives extends OpenIdDirectives with AuthDirectives {
  self: OpenIdActors =>

  import PageDirectives._

  private def loginPage(params: Map[String, String], lid: Option[LocalId], endpointUrl: String, failed: Boolean) = {
    import Implicits._
    val context = defaultContext ++ Map(
      "realm" -> params.get(Keys.realm.openid).getOrElse(""),
      "identity" -> params.get(Keys.identity.openid).getOrElse(""),
      "endpointUrl" -> endpointUrl,
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
      "endpointUrl" -> settings.endpointUrl.toString(),
      "params" -> (params ++ returnto.query.toMap).map(t => Map("name" -> t._1, "value"->t._2))
    )
    settings.continueTemplate(context)
  }

  private def errorPage(params: Map[String, String]) = {
    val context = defaultContext ++ Map("params" -> params.map(t => Map("name" -> t._1, "value"->t._2)))
    settings.errorTemplate(context)
  }

  def renderLoginPage(endpointUrl: String, failed: Boolean) = allParams { req =>
    localIdOption { lidopt =>
      removePorterCookie() {
        complete(HttpResponse(
          entity = HttpEntity(html,
            loginPage(req, lidopt, endpointUrl, failed))))
      }
    }
  }

  def renderErrorPage = allParams { params =>
    complete(HttpResponse(
      status = StatusCodes.BadRequest,
      entity = HttpEntity(html, errorPage(params))))
  }

  def renderContinuePage(params: Map[String, String]) = returnToUrl { uri =>
    val localid = params.get(Keys.identity.openid) match {
      case Some(LocalIdParts(lid)) => lid
      case _ => sys.error("Invalid positive assertion! Does not contain openid.identity attribute")
    }
    val paramsWithId = params
      .updated("porter.realm", localid.realm.name)
      .updated("porter.account", localid.account.name)
    complete(HttpResponse(
      entity = HttpEntity(html, continueForm(uri, paramsWithId))))
  } ~ renderErrorPage

}

object PageDirectives {

  val html = ContentType(MediaTypes.`text/html`)

  val defaultContext = Map(
    "porter" -> Map("version" -> BuildInfo.version,
      "revision" -> BuildInfo.revision,
      "builtTime" -> new java.util.Date(BuildInfo.buildTime))
  )

}
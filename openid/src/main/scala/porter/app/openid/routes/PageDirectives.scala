package porter.app.openid.routes

import scala.xml.NodeSeq
import porter.app.openid.common.{LocalId, HtmlTemplates, Keys}
import spray.routing.Directives._
import spray.http._
import porter.app.openid.common
import spray.http.HttpResponse
import porter.app.openid.common.LocalId

trait PageDirectives {
  self: OpenIdDirectives =>

  private def hiddenInputs(parameters: Map[String, String], failedMessage: Boolean) = {
    val message =
      if (failedMessage) {
        <p class="alert alert-danger alert-dismissable"><strong>Error:</strong> Invalid credentials!</p>
      } else NodeSeq.Empty
    val inputs = for ((k, v) <- parameters; if k startsWith "openid") yield {
        <input type="hidden" name={k} value={v} />
    }
    message ++ inputs
  }

  private def continueForm(params: Map[String, String]) =
    <form class="form-horizontal" role="form" action={params.get(Keys.return_to.openid).get} method="post">
      { hiddenInputs(params, failedMessage = false) }
      <div class="form-group">
        <div class="col-sm-offset-2 col-sm-10">
          <input type="submit" name="submitType" class="btn btn-primary" value="Continue"></input>
        </div>
      </div>
    </form>

  private def signinForm(params: Map[String, String], lid: Option[LocalId], failedMessage: Boolean) = {
    import scala.xml.Text
    val id = lid.map(_.account.name)

    <form role="form" action={settings.endpointUrl.path.toString()} method="post">
      { hiddenInputs(params, failedMessage) }
      <div class="form-group">
        <label for="username" class="sr-only">Username</label>
        <input type="text" class="form-control input-lg" id="username" name="username" value={id map Text.apply} placeholder="Username"></input>
      </div>
      <div class="form-group">
        <label for="password" class="sr-only">Password</label>
        <input type="password" class="form-control input-lg" id="password" name="password" placeholder="Password"></input>
      </div>
      <div class="form-group">
        <div class="checkbox">
          <label>
            <input type="checkbox" name="rememberme" checked="checked"> Remember me </input>
          </label>
        </div>
      </div>
      <div class="form-group">
        <input type="submit" name="submitType" class="btn btn-primary btn-lg" value="Sign in"></input>
        <input type="submit" name="submitType" class="btn btn-default btn-lg" value="Cancel"></input>
      </div>
    </form>
  }

  private def loginTemplate(req: Map[String, String], lid: Option[LocalId], failedMessage: Boolean) =
    HtmlTemplates.createLoginPage(signinForm(req, lid, failedMessage),
      <p>You're authenticating with
        <strong>{req.get(Keys.realm.openid).getOrElse("No Realm!")}</strong> as
        <strong>{req(Keys.identity.openid)}</strong></p>)(settings)

  private def continueTemplate(req: Map[String, String]) =
    HtmlTemplates.createContinueTemplate(continueForm(req),
      <p>Continue authentication with
        <strong>{req.get(Keys.realm.openid).getOrElse(req(Keys.return_to.openid))}</strong>.</p>)(settings)

  private def errorTemplate =
    HtmlTemplates.createErrorTemplate(settings)

  def renderLoginPage(failed: Boolean) = allParams { req =>
    localIdOption { lidopt =>
      complete(HttpResponse(
        entity = HttpEntity(ContentType(MediaTypes.`text/html`),
          loginTemplate(req, lidopt, failed).get.get)))
    }
  }

  def renderErrorPage = complete(HttpResponse(
    status = StatusCodes.BadRequest,
    entity = HttpEntity(ContentType(MediaTypes.`text/html`), errorTemplate.get.get)))

  def renderContinuePage(params: Map[String, String]) =
    complete(HttpResponse(
      entity = HttpEntity(ContentType(MediaTypes.`text/html`), continueTemplate(params).get.get)))

}

package porter.app.openid.routes

import spray.routing._
import spray.http._
import spray.httpx.marshalling.{ToResponseMarshallingContext, ToResponseMarshallable}
import scala.xml.{NodeSeq, Elem}
import spray.http.HttpResponse

trait DiscoveryRoute extends OpenIdDirectives {
  self: OpenIdActors =>

  import spray.routing.Directives._
  import scala.language.implicitConversions
  import porter.app.openid.common._

  private implicit def xmlResponse(xml: NodeSeq): ToResponseMarshallable = new ToResponseMarshallable {
    val ct = ContentType(DiscoveryRoute.`application/xrds+xml`)
    def marshal(ctx: ToResponseMarshallingContext) =
      ctx.marshalTo(HttpResponse(entity = HttpEntity(ct, xml.toString())))
  }

  def xrdsServer: NodeSeq = {
    <xrds:XRDS xmlns:xrds="xri://$xrds" xmlns="xri://$xrd*($v*2.0)">
    <XRD>
    <Service priority="0">
    <Type>{ openid20 +"/server" }</Type>
    <Type>{ sreg10Ns }</Type>
    <URI>{settings.endpointUrl.toString()}</URI>
    </Service>
    </XRD>
    </xrds:XRDS>
  }

  def xrdsSignon(id: String):NodeSeq = {
    <xrds:XRDS xmlns:xrds="xri://$xrds" xmlns="xri://$xrd*($v*2.0)">
    <XRD>
    <Service priority="0">
    <Type>{ openid20 +"/signon" }</Type>
    <Type>{ sreg10Ns }</Type>
    <URI>{settings.endpointUrl.toString()}</URI>
    </Service>
    </XRD>
    </xrds:XRDS>
  }
  def discovery: Route = {
    path(Segment ~ PathEnd) { id =>
      complete(xrdsSignon(id))
    } ~
    path(Segment / Segment ~ PathEnd) { (r, u) =>
      if (r == "openid") reject()
      else complete(xrdsSignon(u))
    } ~
    path("openid" / "login") {
      complete(xrdsServer)
    }
  }
}

object DiscoveryRoute {

  lazy val `application/xrds+xml` = {
    val mt = MediaType.custom(mainType = "application", subType = "xrds+xml")
    MediaTypes.register(mt)
    mt
  }
}
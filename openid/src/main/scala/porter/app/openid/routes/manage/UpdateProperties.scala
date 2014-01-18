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

package porter.app.openid.routes.manage

import porter.app.openid.routes.OpenIdActors
import spray.routing.Directives._
import spray.http.{HttpHeader, HttpHeaders, BodyPart, MultipartFormData}
import porter.model._
import porter.client.Messages.mutableStore.UpdateAccount
import porter.client.Messages.mutableStore.OperationFinished
import scala.util.Success
import porter.model.Property.BinaryValue
import akka.pattern.ask

trait UpdateProperties {
  self: ManageRoutes with OpenIdActors =>

  def changePropertiesSubmission: Submission = {
    case Action("updateProperties", ctx, acc) =>
      entity(as[MultipartFormData]) { data =>
        updateProperties(data) match {
          case Right(fun) =>
            val nacc = acc.updatedProps(fun)
            val upd = UpdateAccount(settings.defaultRealm, nacc)
            onComplete((porterRef ? upd).mapTo[OperationFinished]) {
              case Success(OperationFinished(true)) => renderUserPage(nacc, Message.success("Account saved."))
              case _ => renderUserPage(acc, Message.error("Error saving account."))
            }
          case Left(msgs) =>
            renderUserPage(acc, Message.error("Invalid property values.", msgs))
        }
      }

  }

  private def findContentType(headers: Seq[HttpHeader]) =
    headers.collect{ case HttpHeaders.`Content-Type`(c) if c.mediaType.isImage => c.toString() }.headOption

  private def validateProperties(props: Seq[Property[_]], data: MultipartFormData): List[String] = {
    val birthdate = """(\d{4}\-\d{2}\-\d{2})""".r
    props.map {
      case PropertyList.avatar =>
        val body = data.get(PropertyList.avatar.name).get
        val len = body.entity.data.length
        val ct = findContentType(body.headers)
        if (len > settings.avatarMaxUploadSize * 1024) s"Image is too large, only ${settings.avatarMaxUploadSize}kb are allowed."
        else if (ct.isEmpty) "Invalid image file: invalid content type."
        else ""
      case PropertyList.birthday =>
        val in = data.get(PropertyList.birthday.name).get.entity.asString
        in match {
          case birthdate(_) => ""
          case _ => s"Invalid birthdate string '$in'. Use pattern 'yyyy-mm-dd'"
        }
      case PropertyList.email =>
        val in = data.get(PropertyList.email.name).get.entity.asString
        if (in.indexOf('@') < 0) s"'$in' does not seem to be a valid email"
        else ""
      case _ => ""
    }.filter(_.nonEmpty).toList
  }

  private def updateProperties(data: MultipartFormData): Either[List[String], Properties => Properties] = {
    val (add, remove) = PropertyList.userProps.partition(p => data.get(p.name).exists(_.entity.nonEmpty))
    validateProperties(add, data) match {
      case Nil =>
        val fadds = add.map {
          case PropertyList.avatar =>
            val fun = for {
              body <- data.get(PropertyList.avatar.name)
              ct <- findContentType(body.headers)
            } yield PropertyList.avatar.set(BinaryValue(ct, body.entity.data.toByteArray))
            fun getOrElse identity[Properties]_
          case prop =>
            data.get(prop.name).map(bp => prop.setRaw(bp.entity.asString)).getOrElse(identity[Properties]_)
        }
        val fremoves = remove.filterNot(_ == PropertyList.avatar).map(p => p.remove)
        Right((fadds ++ fremoves).reduce(_ andThen _))

      case msgs => Left(msgs)
    }
  }
}

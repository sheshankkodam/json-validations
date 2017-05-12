package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

case class Data(firstName: String,
                lastName: String,
                email: String,
                phone: Long
               )
@Singleton
class HomeController @Inject() extends Controller {

  implicit lazy val dataReads: Reads[Data] = (
    (__ \ "firstName").read[String] and
      (__ \ "lastName").read[String](maxLength[String](10)) and
      (__ \ "email").read[String](email) and
      (__ \ "phone").read[Long]
    ) (Data.apply _)

  implicit val dataWrites: Writes[Data] = (
    (JsPath \ "firstName").write[String] and
      (JsPath \ "lastName").write[String] and
      (JsPath \ "email").write[String] and
      (JsPath \ "phone").write[Long]

    ) (unlift(Data.unapply))

  def jsonCombinators = Action(parse.json) { request =>
    val json = request.body
    json.validate[Data].fold(
      valid = res => Ok(Json.toJson(res)),
      invalid = e => BadRequest(e.toString)
    )
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}

package controllers

import javax.inject._

import modules.{EmployeeData, ResponseReaderWriter}
import play.api.mvc._
import play.api.libs.json._

@Singleton
class CombinatorsController @Inject()(responseReaderWriter: ResponseReaderWriter) extends Controller {
  implicit val employeeDataReads = responseReaderWriter.dataReads
  implicit val employeeDataWrites = responseReaderWriter.dataWrites

  def jsonCombinators = Action(parse.json) { request =>
    val json = request.body
    json.validate[EmployeeData].fold(
      valid = res => Ok(Json.toJson(res)),
      invalid = e => BadRequest(e.toString)
    )
  }
}

package controllers

import javax.inject._

import modules.{EmployeeData, ResponseReaderWriter}
import play.api.libs.json.Json
import play.api.mvc._

@Singleton
class CatsController @Inject()(responseReaderWriter: ResponseReaderWriter) extends Controller {
  implicit val employeeDataWrites = responseReaderWriter.dataWrites

  def validateFirstNameWithCats(name: String): Option[String] = {
    name match {
      case s: String => Some(s)
      case _ => None
    }}

  def validateLastNameWithCats(name: String): Option[String] = {
    name match {
      case s: String => Some(s)
      case _ => None
    }}

  def validateEmailWithCats(email: String): Option[String] = {
    email match {
      case m: String if m contains ".com" => Some(m)
      case _ => Some("Bad email")
    }}

  def validatePhoneWithCats(number: Long): Option[String] = {
    number match {
      case n: Long if n.toString.length == 10 => Some(n.toString)
      case _ => None
    }}

  /**
    * Problems:
    *   -  Good and bad data are combined
    *   - When used None, it returns null
    * @return
    */

  def validateWithOptions = Action(parse.json) { request =>
    val json = request.body
    val firstName = (json \ "firstName").as[String]
    val lastName = (json \ "lastName").as[String]
    val email = (json \ "email").as[String]
    val phone = (json \ "phone").as[Long]

    Ok("Done")
  }
}

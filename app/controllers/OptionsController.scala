package controllers

import javax.inject._

import modules.{EmployeeData, ResponseReaderWriter}
import play.api.libs.json.Json
import play.api.mvc._

@Singleton
class OptionsController @Inject() (responseReaderWriter: ResponseReaderWriter) extends Controller {
  implicit val employeeDataWrites = responseReaderWriter.dataWrites

  def validateFirstName(name: String): Option[String] = {
    name match {
      case s: String if s.length > 0 => Some(s)
      case _ => Some("Invalid first name")
    }}

  def validateLastName(name: String): Option[String] = {
    name match {
      case s: String if s.length > 0 => Some(s)
      case _ => Some("Invalid last name")
    }}

  def validateEmail(email: String): Option[String] = {
    email match {
      case m: String if m contains ".com" => Some(m)
      case _ => Some("Invalid email")
    }}

  def validatePhone(number: Long): Option[Long] = {
    number match {
      case n: Long if n.toString.length == 10 => Some(n)
      case _ => Some(0)
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

    val data = for {
      validFirstName <- validateFirstName(firstName)
      validLastName  <- validateLastName(lastName)
      validPhone     <- validatePhone(phone)
      validEmail     <- validateEmail(email)
    } yield EmployeeData(validFirstName, validLastName, validEmail, validPhone)
    Ok(Json.toJson(data))
  }
}

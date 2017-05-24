package controllers

import javax.inject._

import modules.{EmployeeData, ResponseReaderWriter}
import play.api.libs.json.Json
import play.api.mvc._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._


@Singleton
class CatsController @Inject()(responseReaderWriter: ResponseReaderWriter) extends Controller {
  implicit val employeeDataWrites = responseReaderWriter.dataWrites

  def validateDataWithCats(d:EmployeeData):Validated[List[String], EmployeeData] = {
    val validFirstName = validateFirstNameWithCats(d.firstName)
    val validLastName = validateLastNameWithCats(d.lastName)
    val validEmail = validateEmailWithCats(d.email)
    val validPhone = validatePhoneWithCats(d.phone)

    (validFirstName |@| validLastName |@| validEmail |@| validPhone).map(EmployeeData)
  }

  def validateFirstNameWithCats(name: String):Validated[List[String], String] = {
    name match {
      case s: String if s.length > 0 => Validated.valid(s)
      case _ => Validated.invalid(List("invalid first name"))
    }}

  def validateLastNameWithCats(name: String):Validated[List[String], String] = {
    name match {
      case s: String if s.length > 0 => Validated.valid(s)
      case _ => Validated.invalid(List("invalid last name"))
    }}

  def validateEmailWithCats(email: String): Validated[List[String], String] = {
    email match {
      case m: String if m contains ".com" => Validated.valid(m)
      case _ => Validated.invalid(List("invalid email"))
    }}

  def validatePhoneWithCats(number: Long): Validated[List[String], Long] = {
    number match {
      case n: Long if n.toString.length == 10 => Validated.valid(n)
      case _ => Validated.invalid(List("Invalid phone number"))
    }}

  def validateWithCats = Action(parse.json) { request =>
    val json = request.body

    val firstName = (json \ "firstName").as[String]
    val lastName = (json \ "lastName").as[String]
    val email = (json \ "email").as[String]
    val phone = (json \ "phone").as[Long]

    val data = EmployeeData(firstName, lastName, email, phone)
    validateDataWithCats(data) match {
      case Invalid(e) => BadRequest(Json.toJson(e))
      case Valid(d) => Ok(Json.toJson(d))
    }
  }
}

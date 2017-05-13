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

  def validateFirstNameWithCats(name: String):Validated[List[String], String] = {
    name match {
      case s: String if s.length > 0 => Validated.valid(s)
      case _ => Validated.invalid(List("invalid first name"))
    }}

  def validateLastNameWithCats(name: String):Validated[List[String], String] = {
    name match {
      case s: String => Validated.valid(s)
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
    val validFirstName = validateFirstNameWithCats((json \ "firstName").as[String])
    val validLastName = validateLastNameWithCats((json \ "lastName").as[String])
    val validEmail = validateEmailWithCats((json \ "email").as[String])
    val validPhone = validatePhoneWithCats((json \ "phone").as[Long])

    val result = (validFirstName |@| validLastName |@| validEmail |@| validPhone).map{
      case (fn, ln, em, ph) => new EmployeeData(fn, ln, em, ph)
    }
    result match {
      case Invalid(e) => BadRequest(e.toString)
      case Valid(d) => Ok(Json.toJson(d))
    }
  }
}

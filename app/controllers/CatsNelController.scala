package controllers

import javax.inject._

import cats.data._
import cats.data.{Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import modules.{EmployeeData, Err, ResponseReaderWriter}
import play.api.libs.json.Json
import play.api.mvc._
import cats.implicits._

@Singleton
class CatsNelController @Inject()(responseReaderWriter: ResponseReaderWriter) extends Controller {
  implicit val employeeDataWrites = responseReaderWriter.dataWrites
  implicit val errWrites = responseReaderWriter.errWrites


  def validateFirstNameWithCats(name: String):ValidatedNel[Err, String] = {
    name match {
      case s: String if s.length > 0 => Validated.valid(s)
      case _ => Validated.invalidNel(Err(ErrCode = "Empty First Name", ErrMsg = "Invalid First Name"))
    }}

  def validateLastNameWithCats(name: String):ValidatedNel[Err, String] = {
    name match {
      case s: String if s.length > 0 => Validated.valid(s)
      case _ =>  Validated.invalidNel(Err(ErrCode = "Empty Last Name", ErrMsg = "Invalid Last Name"))
    }}

  def validateEmailWithCats(email: String): ValidatedNel[Err, String] = {
    email match {
      case m: String if m contains ".com" => Validated.valid(m)
      case _ => Validated.invalidNel(Err(ErrCode = "Invalid Email Format", ErrMsg = "Invalid Email"))
    }}

  def validatePhoneWithCats(number: Long): ValidatedNel[Err, Long] = {
    number match {
      case n: Long if n.toString.length == 10 => Validated.valid(n)
      case _ => Validated.invalidNel(Err(ErrCode = "Phone number too short", ErrMsg = "Invalid Phone"))
    }}

  def validateData(d:EmployeeData):ValidatedNel[Err, EmployeeData] = {
    val validFirstName = validateFirstNameWithCats(d.firstName)
    val validLastName = validateLastNameWithCats(d.lastName)
    val validEmail = validateEmailWithCats(d.email)
    val validPhone = validatePhoneWithCats(d.phone)

    (validFirstName |@| validLastName |@| validEmail |@| validPhone).map(EmployeeData)

  }

  def validateWithCatsNel = Action(parse.json) { request =>
    val json = request.body
    val firstName = (json \ "firstName").as[String]
    val lastName = (json \ "lastName").as[String]
    val email = (json \ "email").as[String]
    val phone = (json \ "phone").as[Long]

    val data = EmployeeData(firstName, lastName, email, phone)
    validateData(data) match {
      case Invalid(e) => BadRequest(Json.toJson(e.toList))
      case Valid(d) => Ok(Json.toJson(d))
    }
  }
}

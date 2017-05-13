package controllers

import javax.inject._

import modules.{EmployeeData, ResponseReaderWriter}
import play.api.libs.json.Json
import play.api.mvc._

@Singleton
class EitherController @Inject()(responseReaderWriter: ResponseReaderWriter) extends Controller {
  implicit val employeeDataWrites = responseReaderWriter.dataWrites

  def validateFirstNameWithEither(name: String): Either[Option[String],Option[String]] = {
    name match {
      case s: String => Right(Some(s))
      case _ => Left(Some("Bad first name"))
    }}

  def validateLastNameWithEither(name: String): Either[Option[String],Option[String]] = {
    name match {
      case s: String => Right(Some(s))
      case _ => Left(Some("Bad last name"))
    }}

  def validateEmailWithEither(email: String):Either[Option[String],Option[String]] = {
    email match {
      case m: String if m contains ".com" => Right(Some(m))
      case _ =>  Left(Some("Bad email"))
    }}

  def validatePhoneWithEither(number: Long): Either[Option[String],Option[String]] = {
    number match {
      case n: Long if n.toString.length == 10 => Right(Some(n.toString))
      case _ =>  Left(Some("Bad phone"))
    }}

  /**
    * Problems:
    *   -  Good and bad data are combined
    *   - When used None, it returns null
    *
    * @return
    */

  def validateWithEither = Action(parse.json) { request =>
    val json = request.body
    val firstName = (json \ "firstName").as[String]
    val lastName = (json \ "lastName").as[String]
    val email = (json \ "email").as[String]
    val phone = (json \ "phone").as[Long]

    val data = for {
      validFirstName <- validateFirstNameWithEither(firstName).right
      validLastName  <- validateLastNameWithEither(lastName).right
      validEmail     <- validateEmailWithEither(email).right
      validPhone     <- validatePhoneWithEither(phone).right
    } yield EmployeeData(validFirstName.get, validLastName.get, validEmail.get, validPhone.get.toLong)

    data match {
      case Right(d) => Ok(Json.toJson(d))
      case Left(e)  => Ok(Json.toJson(e.get.toString))
    }
  }
}

package modules

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

/**
  * Created by sheshank.kodam on 5/12/17.
  */
class ResponseReaderWriter{
  implicit lazy val dataReads: Reads[EmployeeData] = (
    (__ \ "firstName").read[String] and
      (__ \ "lastName").read[String](maxLength[String](10)) and
      (__ \ "email").read[String](email) and
      (__ \ "phone").read[Long]
    ) (EmployeeData.apply _)

  implicit val dataWrites: Writes[EmployeeData] = (
    (JsPath \ "firstName").write[String] and
      (JsPath \ "lastName").write[String] and
      (JsPath \ "email").write[String] and
      (JsPath \ "phone").write[Long]
    ) (unlift(EmployeeData.unapply))

  implicit val errWrites: Writes[Err] = (
    (__ \ "ErrCode").write[String] and
      (__ \ "ErrMsg").write[String]
    ) (unlift(Err.unapply))
}

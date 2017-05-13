package controllers

import javax.inject._
import play.api.mvc._

@Singleton
class OptionsController @Inject() extends Controller {
  def validateWithOptions = Action(parse.json) { request =>
    val json = request.body
    Ok("Good")
  }
}

package brbo.backend2

import brbo.common.BrboType
import brbo.common.ast._
import play.api.libs.json._

object InputParser {
  def parse(content: String): List[List[BrboValue]] = {
    Json.parse(content) match {
      case JsArray(listOfInputs) =>
        listOfInputs.map({
          case JsArray(parameters) =>
            parameters.map({ parameter => parse(parameter) }).toList
          case _ => throw new Exception
        }).toList
      case _ => throw new Exception
    }
  }

  def parse(jsValue: JsValue): BrboValue = {
    jsValue match {
      case boolean: JsBoolean => Bool(boolean.value)
      case JsNumber(value) => Number(value.toInt)
      case JsArray(array) =>
        val values: List[Number] = array.map({
          element => parse(element).asInstanceOf[Number]
        }).toList
        BrboArray(values, innerType = BrboType.INT)
      case JsString(_) | JsObject(_) => throw new Exception
    }
  }
}

package brbo.common.ast

/*abstract class BrboAnf extends PrettyPrintPrintToCFG

abstract class EXPRESSION extends BrboAnf

abstract class VALUE extends EXPRESSION

case class VARIABLE(identifier: String, typ: BrboType) extends VALUE {
  override def prettyPrintPrintToCFG: String = identifier
}

case class NUMBER(n: Int) extends VALUE {
  override def prettyPrintPrintToCFG: String = n.toString
}

case class BOOLEAN(b: Boolean) extends VALUE {
  override def prettyPrintPrintToCFG: String = b.toString
}

case class LET_VARIABLE(variable: VARIABLE, value: VALUE, body: EXPRESSION) extends EXPRESSION {
  override def prettyPrintPrintToCFG: String = s"let ${variable.prettyPrintPrintToCFG} = ${value.prettyPrintPrintToCFG} in ${body.prettyPrintPrintToCFG}"
}

case class LET_FUNCTION(variable: VARIABLE, identifier: String, arguments: List[VARIABLE], returnType: BrboType, body: EXPRESSION) extends EXPRESSION {
  override def prettyPrintPrintToCFG: String = {
    val argumentsString = arguments.map(a => a.prettyPrintPrintToCFG).mkString(", ")
    s"let ${variable.prettyPrintPrintToCFG} = $identifier($argumentsString) in ${body.prettyPrintPrintToCFG}"
  }
}*/

trait GetFunctionCalls {
  def getFunctionCalls: List[FunctionCallExpr]
}
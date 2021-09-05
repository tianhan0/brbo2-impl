package brbo.common.ast

import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BrboType, INT, BOOL, VOID}

sealed trait BrboAst {
  val DEFAULT_INDENT = 2

  def prettyPrintToC(indent: Int): String
}

sealed trait Command extends BrboAst

sealed trait Statement extends BrboAst

case class TypeNamePair(identifier: String, typ: BrboType) {
  def prettyPrintToC(): String = s"${BrboType.toCString(typ)} $identifier"
}

case class Function(identifier: String, returnType: BrboType, parameters: List[TypeNamePair], body: Statement) extends BrboAst {
  override def prettyPrintToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.prettyPrintToC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) {\n${body.prettyPrintToC(DEFAULT_INDENT)}\n}"
  }
}

case class Block(statements: List[BrboAst]) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString{\n${statements.map(t => t.prettyPrintToC(indent + DEFAULT_INDENT)).mkString("\n")}\n$indentString}"
  }
}

case class Loop(condition: BrboExpr, body: BrboAst) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val bodyString = s"${body.prettyPrintToC(indent)}"
    val indentString = " " * indent
    s"${indentString}while $conditionString\n$bodyString"
  }
}

case class ITE(condition: BrboExpr, thenAst: BrboAst, elseAst: BrboAst) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val thenString = thenAst.prettyPrintToC(indent + DEFAULT_INDENT)
    val elseString = elseAst.prettyPrintToC(indent + DEFAULT_INDENT)
    val indentString = " " * indent
    s"${indentString}if($conditionString)\n$thenString\n${indentString}else\n$elseString"
  }
}

case class Assert(condition: BrboExpr) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val indentString = " " * indent
    s"${indentString}assert($conditionString);"
  }
}

case class VariableDeclaration(variable: String, typ: BrboType, initialValue: BrboExpr) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val initialValueString =
      typ match {
      case INT => assert(initialValue.typ == INT); initialValue.prettyPrintToC()
      case BOOL => assert(initialValue.typ == BOOL); initialValue.prettyPrintToC()
      case VOID => throw new Exception
    }
    val indentString = " " * indent
    s"$indentString${BrboType.toCString(typ)} $variable = $initialValueString;"
  }
}

case class Assignment(variable: String, expression: BrboExpr) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString$variable = ${expression.prettyPrintToC()};"
  }
}

case class FunctionCall(functionCallExpr: FunctionCallExpr) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${functionCallExpr.prettyPrintToC()};"
  }
}

case object Return extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}return;"
  }
}

case object Skip extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString;"
  }
}

case object Break extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}break;"
  }
}

case object Continue extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}continue;"
  }
}
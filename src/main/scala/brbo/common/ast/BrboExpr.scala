package brbo.common.ast

import brbo.common.BrboType
import brbo.common.BrboType.{BOOL, BrboType, INT}

import java.util.UUID

abstract class BrboExpr(val typ: BrboType) extends PrettyPrintToC with PrettyPrintToCFG with GetFunctionCalls {
  override def prettyPrintToC(indent: Int): String

  override def toString: String = prettyPrintToC()

  def prettyPrintToCNoOuterBrackets: String = {
    val string = prettyPrintToC()
    if (string.startsWith("(") && string.endsWith(")")) string.substring(1, string.length - 1)
    else string
  }
}

case class Identifier(identifier: String, override val typ: BrboType, uuid: UUID = UUID.randomUUID()) extends BrboExpr(typ) {
  override def prettyPrintToC(indent: Int): String = identifier

  def typeNamePairInC(): String = s"${BrboType.toCString(typ)} $identifier"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class Bool(b: Boolean, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  override def prettyPrintToC(indent: Int): String = b.toString

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class Number(n: Int, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  override def prettyPrintToC(indent: Int): String = n.toString

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class Addition(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} + ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class Subtraction(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} - ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class Multiplication(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} * ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class Division(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} / ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class Negative(expression: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(expression.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"!(${expression.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = expression.getFunctionCalls
}

case class LessThan(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} < ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class LessThanOrEqualTo(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} <= ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class GreaterThan(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} > ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class GreaterThanOrEqualTo(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} >= ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class Equal(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == right.typ)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} == ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class NotEqual(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == right.typ)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} != ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class And(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} && ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class Or(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} || ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls
}

case class FunctionCallExpr(identifier: String, arguments: List[BrboExpr], returnType: BrboType, uuid: UUID = UUID.randomUUID()) extends BrboExpr(returnType) {
  override def prettyPrintToC(indent: Int): String = {
    val argumentsString = arguments.map(argument => argument.prettyPrintToCNoOuterBrackets).mkString(", ")
    s"$identifier($argumentsString)"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = {
    arguments.flatMap(argument => argument.getFunctionCalls) :+ this
  }
}

case class ITEExpr(condition: BrboExpr, thenExpr: BrboExpr, elseExpr: BrboExpr) extends BrboExpr(thenExpr.typ) {
  assert(condition.typ == BOOL)
  assert(thenExpr.typ == elseExpr.typ)

  override def prettyPrintToC(indent: Int): String = s"${condition.prettyPrintToC()} ? ${thenExpr.prettyPrintToC()} : ${elseExpr.prettyPrintToC()}"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = condition.getFunctionCalls ::: thenExpr.getFunctionCalls ::: elseExpr.getFunctionCalls
}
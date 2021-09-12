package brbo.common.ast

import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}

import java.util.UUID

abstract class BrboExpr(val typ: BrboType) extends PrettyPrintToC with PrettyPrintPrintToCFG {
  override def prettyPrintToC(indent: Int): String
}

case class Identifier(identifier: String, typ2: BrboType, uuid: UUID = UUID.randomUUID()) extends BrboExpr(typ2) {
  override def prettyPrintToC(indent: Int): String = identifier

  def typeNamePairInC(): String = s"${BrboType.toCString(typ)} $identifier"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Bool(b: Boolean, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  override def prettyPrintToC(indent: Int): String = b.toString

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Number(n: Int, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  override def prettyPrintToC(indent: Int): String = n.toString

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Addition(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} + ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Subtraction(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} - ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Multiplication(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} * ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Division(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} / ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Negative(expression: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(expression.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"!(${expression.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class LessThan(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} < ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class LessThanOrEqualTo(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} <= ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class GreaterThan(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} > ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class GreaterThanOrEqualTo(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} >= ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Equal(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == right.typ)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} == ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class NotEqual(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == right.typ)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} != ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class And(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} && ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class Or(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} || ${right.prettyPrintToC()})"

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}

case class FunctionCallExpr(identifier: String, arguments: List[BrboExpr], returnType: BrboType, uuid: UUID = UUID.randomUUID()) extends BrboExpr(returnType) {
  override def prettyPrintToC(indent: Int): String = {
    val argumentsString = arguments.map(argument => argument.prettyPrintToC()).mkString(", ")
    s"$identifier($argumentsString)"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC(0)

}
package brbo.common.ast

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}

abstract class BrboExpr(val typ: BrboType) {
  def prettyPrintToC(): String
}

case class Identifier(identifier: String, typ2: BrboType) extends BrboExpr(typ2) {
  def prettyPrintToC(): String = identifier
}

case class Bool(b: Boolean) extends BrboExpr(BOOL) {
  def prettyPrintToC(): String = b.toString
}

case class Number(n: Int) extends BrboExpr(INT) {
  def prettyPrintToC(): String = n.toString
}

case class Addition(left: BrboExpr, right: BrboExpr) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} + ${right.prettyPrintToC()})"
}

case class Subtraction(left: BrboExpr, right: BrboExpr) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} - ${right.prettyPrintToC()})"
}

case class Multiplication(left: BrboExpr, right: BrboExpr) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} * ${right.prettyPrintToC()})"
}

case class Division(left: BrboExpr, right: BrboExpr) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} / ${right.prettyPrintToC()})"
}

case class Negative(expression: BrboExpr) extends BrboExpr(BOOL) {
  assert(expression.typ == BOOL)

  def prettyPrintToC(): String = s"!(${expression.prettyPrintToC()})"
}

case class LessThan(left: BrboExpr, right: BrboExpr) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} < ${right.prettyPrintToC()})"
}

case class LessThanOrEqualTo(left: BrboExpr, right: BrboExpr) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} <= ${right.prettyPrintToC()})"
}

case class GreaterThan(left: BrboExpr, right: BrboExpr) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} > ${right.prettyPrintToC()})"
}

case class GreaterThanOrEqualTo(left: BrboExpr, right: BrboExpr) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} >= ${right.prettyPrintToC()})"
}

case class Equal(left: BrboExpr, right: BrboExpr) extends BrboExpr(BOOL) {
  assert(left.typ == right.typ)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} == ${right.prettyPrintToC()})"
}

case class NotEqual(left: BrboExpr, right: BrboExpr) extends BrboExpr(BOOL) {
  assert(left.typ == right.typ)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} != ${right.prettyPrintToC()})"
}

case class And(left: BrboExpr, right: BrboExpr) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} && ${right.prettyPrintToC()})"
}

case class Or(left: BrboExpr, right: BrboExpr) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  def prettyPrintToC(): String = s"(${left.prettyPrintToC()} || ${right.prettyPrintToC()})"
}

case class FunctionCallExpr(identifier: String, arguments: List[String], returnType: BrboType) extends BrboExpr(returnType) {
  override def prettyPrintToC(): String = {
    val argumentsString = arguments.mkString(", ")
    s"$identifier($argumentsString)"
  }
}
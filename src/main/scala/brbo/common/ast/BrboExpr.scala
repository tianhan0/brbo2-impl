package brbo.common.ast

import brbo.common.BrboType.{ARRAY, BOOL, INT, STRING}
import brbo.common.{BrboType, Z3Solver}
import com.microsoft.z3.AST

import java.util.UUID

trait BrboValue

abstract class BrboExpr(val typ: BrboType.T) extends CommandOrExpr with PrettyPrintToC
  with GetFunctionCalls with Z3AST with UseDefVariables with UniqueCopy {
  override def prettyPrintToC(indent: Int): String

  override def toString: String = prettyPrintToC()

  def prettyPrintToCNoOuterBrackets: String = {
    val string = prettyPrintToC()
    if (string.startsWith("(") && string.endsWith(")")) string.substring(1, string.length - 1)
    else string
  }
}

case class Identifier(name: String, override val typ: BrboType.T, uuid: UUID = UUID.randomUUID()) extends BrboExpr(typ) {
  override def prettyPrintToC(indent: Int): String = name

  def typeNamePairInC(): String = s"${BrboType.toCString(typ)} $name"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def toZ3AST(solver: Z3Solver): AST = {
    typ match {
      case INT => solver.mkIntVar(name)
      case BOOL => solver.mkBoolVar(name)
      case _ => throw new Exception
    }
  }

  override def getUses: Set[Identifier] = Set(this)

  override def getDefs: Set[Identifier] = Set()

  override def uniqueCopyExpr: BrboExpr = Identifier(name, typ)

  def sameAs(other: Any): Boolean = {
    other match {
      case Identifier(otherName, otherTyp, _) => otherName == name && otherTyp == typ
      case _ => false
    }
  }
}

case class StringLiteral(value: String, uuid: UUID = UUID.randomUUID()) extends BrboExpr(STRING) with BrboValue {
  override def prettyPrintToC(indent: Int): String = value

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def toZ3AST(solver: Z3Solver): AST = ???

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def uniqueCopyExpr: BrboExpr = StringLiteral(value)

  def sameAs(other: Any): Boolean = {
    other match {
      case StringLiteral(otherValue, _) => otherValue == value
      case _ => false
    }
  }
}

case class Bool(b: Boolean, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) with BrboValue {
  override def prettyPrintToC(indent: Int): String = b.toString

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def toZ3AST(solver: Z3Solver): AST = solver.mkBoolVal(b)

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def uniqueCopyExpr: BrboExpr = Bool(b)

  def sameAs(other: Any): Boolean = {
    other match {
      case Bool(otherB, _) => otherB == b
      case _ => false
    }
  }
}

case class Number(n: Int, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) with BrboValue {
  override def prettyPrintToC(indent: Int): String = n.toString

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def toZ3AST(solver: Z3Solver): AST = solver.mkIntVal(n)

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def uniqueCopyExpr: BrboExpr = Number(n)

  def sameAs(other: Any): Boolean = {
    other match {
      case Number(otherN, _) => otherN == n
      case _ => false
    }
  }
}

case class BrboArray(values: List[BrboExpr], innerType: BrboType.T, uuid: UUID = UUID.randomUUID()) extends BrboExpr(ARRAY(innerType)) with BrboValue {
  assert(values.forall(v => v.isInstanceOf[BrboValue] && v.typ == innerType))

  override def prettyPrintToC(indent: Int): String = ???

  override def sameAs(other: Any): Boolean = {
    other match {
      case BrboArray(otherValues, _, _) =>
        if (values.length != otherValues.length)
          false
        else {
          values.zip(otherValues).forall({ case (v1, v2) => v1.sameAs(v2) })
        }
      case _ => false
    }
  }

  override def toZ3AST(solver: Z3Solver): AST = ???

  override def uniqueCopyExpr: BrboExpr = BrboArray(values, innerType)

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def prettyPrintToCFG: String = s"Array(${values.map(v => v.prettyPrintToCFG).mkString(",")})"
}

case class Read(array: BrboExpr, index: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(array.typ.asInstanceOf[BrboType.ARRAY].typ) {
  override def prettyPrintToC(indent: Int): String = ???

  override def sameAs(other: Any): Boolean = {
    other match {
      case Read(otherArray, otherIndex, _) => array.sameAs(otherArray) && index.sameAs(otherIndex)
      case _ => false
    }
  }

  override def toZ3AST(solver: Z3Solver): AST = ???

  override def uniqueCopyExpr: BrboExpr = Read(array, index)

  override def getFunctionCalls: List[FunctionCallExpr] = array.getFunctionCalls ::: index.getFunctionCalls

  override def getUses: Set[Identifier] = array.getUses ++ index.getUses

  override def getDefs: Set[Identifier] = array.getDefs ++ index.getDefs

  override def prettyPrintToCFG: String = ???
}

case class Length(array: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(array.typ.isInstanceOf[BrboType.ARRAY])

  override def prettyPrintToC(indent: Int): String = ???

  override def sameAs(other: Any): Boolean = {
    other match {
      case Length(otherArray, _) => array.sameAs(otherArray)
      case _ => false
    }
  }

  override def toZ3AST(solver: Z3Solver): AST = ???

  override def uniqueCopyExpr: BrboExpr = Length(array)

  override def getFunctionCalls: List[FunctionCallExpr] = array.getFunctionCalls

  override def getUses: Set[Identifier] = array.getUses

  override def getDefs: Set[Identifier] = array.getDefs

  override def prettyPrintToCFG: String = ???
}

case class Addition(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} + ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkAdd(left.toZ3AST(solver), right.toZ3AST(solver))

  override def getUses: Set[Identifier] = left.getUses ++ right.getUses

  override def getDefs: Set[Identifier] = left.getDefs ++ right.getDefs

  override def uniqueCopyExpr: BrboExpr = Addition(left, right)

  def sameAs(other: Any): Boolean = {
    other match {
      case Addition(otherLeft, otherRight, _) => otherLeft.sameAs(left) && otherRight.sameAs(right)
      case _ => false
    }
  }
}

case class Subtraction(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} - ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkSub(left.toZ3AST(solver), right.toZ3AST(solver))

  override def getUses: Set[Identifier] = left.getUses ++ right.getUses

  override def getDefs: Set[Identifier] = left.getDefs ++ right.getDefs

  override def uniqueCopyExpr: BrboExpr = Subtraction(left, right)

  def sameAs(other: Any): Boolean = {
    other match {
      case Subtraction(otherLeft, otherRight, _) => otherLeft.sameAs(left) && otherRight.sameAs(right)
      case _ => false
    }
  }
}

case class Multiplication(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} * ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkMul(left.toZ3AST(solver), right.toZ3AST(solver))

  override def getUses: Set[Identifier] = left.getUses ++ right.getUses

  override def getDefs: Set[Identifier] = left.getDefs ++ right.getDefs

  override def uniqueCopyExpr: BrboExpr = Multiplication(left, right)

  def sameAs(other: Any): Boolean = {
    other match {
      case Multiplication(otherLeft, otherRight, _) => otherLeft.sameAs(left) && otherRight.sameAs(right)
      case _ => false
    }
  }
}

case class Negation(expression: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(expression.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"!(${expression.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = expression.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkNot(expression.toZ3AST(solver))

  override def getUses: Set[Identifier] = expression.getUses

  override def getDefs: Set[Identifier] = expression.getDefs

  override def uniqueCopyExpr: BrboExpr = Negation(expression)

  def sameAs(other: Any): Boolean = {
    other match {
      case Negation(otherExpression, _) => otherExpression.sameAs(expression)
      case _ => false
    }
  }
}

case class LessThan(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} < ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkLt(left.toZ3AST(solver), right.toZ3AST(solver))

  override def getUses: Set[Identifier] = left.getUses ++ right.getUses

  override def getDefs: Set[Identifier] = left.getDefs ++ right.getDefs

  override def uniqueCopyExpr: BrboExpr = LessThan(left, right)

  def sameAs(other: Any): Boolean = {
    other match {
      case LessThan(otherLeft, otherRight, _) => otherLeft.sameAs(left) && otherRight.sameAs(right)
      case _ => false
    }
  }
}

case class Equal(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == right.typ)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} == ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkEq(left.toZ3AST(solver), right.toZ3AST(solver))

  override def getUses: Set[Identifier] = left.getUses ++ right.getUses

  override def getDefs: Set[Identifier] = left.getDefs ++ right.getDefs

  override def uniqueCopyExpr: BrboExpr = Equal(left, right)

  def sameAs(other: Any): Boolean = {
    other match {
      case Equal(otherLeft, otherRight, _) => otherLeft.sameAs(left) && otherRight.sameAs(right)
      case _ => false
    }
  }
}

case class And(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} && ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkAnd(left.toZ3AST(solver), right.toZ3AST(solver))

  override def getUses: Set[Identifier] = left.getUses ++ right.getUses

  override def getDefs: Set[Identifier] = left.getDefs ++ right.getDefs

  override def uniqueCopyExpr: BrboExpr = And(left, right)

  def sameAs(other: Any): Boolean = {
    other match {
      case And(otherLeft, otherRight, _) => otherLeft.sameAs(left) && otherRight.sameAs(right)
      case _ => false
    }
  }
}

case class Or(left: BrboExpr, right: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = s"(${left.prettyPrintToC()} || ${right.prettyPrintToC()})"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = left.getFunctionCalls ::: right.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkOr(left.toZ3AST(solver), right.toZ3AST(solver))

  override def getUses: Set[Identifier] = left.getUses ++ right.getUses

  override def getDefs: Set[Identifier] = left.getDefs ++ right.getDefs

  override def uniqueCopyExpr: BrboExpr = Or(left, right)

  def sameAs(other: Any): Boolean = {
    other match {
      case Or(otherLeft, otherRight, _) => otherLeft.sameAs(left) && otherRight.sameAs(right)
      case _ => false
    }
  }
}

case class FunctionCallExpr(identifier: String, arguments: List[BrboExpr], returnType: BrboType.T, uuid: UUID = UUID.randomUUID()) extends BrboExpr(returnType) {
  override def prettyPrintToC(indent: Int): String = {
    val argumentsString = arguments.map(argument => argument.prettyPrintToCNoOuterBrackets).mkString(", ")
    s"$identifier($argumentsString)"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = {
    arguments.flatMap(argument => argument.getFunctionCalls) :+ this
  }

  override def toZ3AST(solver: Z3Solver): AST = throw new Exception

  override def getUses: Set[Identifier] = arguments.flatMap(argument => argument.getUses).toSet

  override def getDefs: Set[Identifier] = arguments.flatMap(argument => argument.getDefs).toSet

  override def uniqueCopyExpr: BrboExpr = FunctionCallExpr(identifier, arguments, returnType)

  def sameAs(other: Any): Boolean = {
    other match {
      case FunctionCallExpr(otherIdentifier, otherArguments, otherReturnType, _) =>
        val sameArguments =
          if (otherArguments.length != arguments.length) false
          else otherArguments.zip(arguments).forall({ case (a1, a2) => a1.sameAs(a2) })
        otherIdentifier == identifier && sameArguments && otherReturnType == returnType
      case _ => false
    }
  }
}

case class ITEExpr(condition: BrboExpr, thenExpr: BrboExpr, elseExpr: BrboExpr, uuid: UUID = UUID.randomUUID()) extends BrboExpr(thenExpr.typ) {
  assert(condition.typ == BOOL)
  assert(thenExpr.typ == elseExpr.typ)

  override def prettyPrintToC(indent: Int): String = s"${condition.prettyPrintToC()} ? ${thenExpr.prettyPrintToC()} : ${elseExpr.prettyPrintToC()}"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = condition.getFunctionCalls ::: thenExpr.getFunctionCalls ::: elseExpr.getFunctionCalls

  override def toZ3AST(solver: Z3Solver): AST = solver.mkITE(condition.toZ3AST(solver), thenExpr.toZ3AST(solver), elseExpr.toZ3AST(solver))

  override def getUses: Set[Identifier] = condition.getUses ++ thenExpr.getUses ++ elseExpr.getUses

  override def getDefs: Set[Identifier] = condition.getDefs ++ thenExpr.getDefs ++ elseExpr.getDefs

  override def uniqueCopyExpr: BrboExpr = ITEExpr(condition, thenExpr, elseExpr)

  def sameAs(other: Any): Boolean = {
    other match {
      case ITEExpr(otherCondition, otherThenExpr, otherElseExpr, _) =>
        otherCondition.sameAs(condition) && otherThenExpr.sameAs(thenExpr) && otherElseExpr.sameAs(elseExpr)
      case _ => false
    }
  }
}
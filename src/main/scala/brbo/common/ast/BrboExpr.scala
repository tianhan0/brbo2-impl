package brbo.common.ast

import brbo.common.BrboType.{ARRAY, BOOL, INT, STRING}
import brbo.common.{BrboType, Z3Solver}
import com.microsoft.z3.AST

import java.util.UUID

trait BrboValue

abstract class BrboExpr(val typ: BrboType.T, uuid: UUID) extends Command(uuid)
  with GetFunctionCalls with Z3AST with UseDefVariables with UniqueCopy {
  override def toString: String = print(0)

  def print(indent: Int): String

  def printNoOuterBrackets: String = {
    val string = print(0)
    if (string.startsWith("(") && string.endsWith(")")) string.substring(1, string.length - 1)
    else string
  }

  override protected def printToCCommand(indent: Int): String = throw new Exception("Unreachable")
}

case class Identifier(name: String, override val typ: BrboType.T, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(typ, uuid) {
  override def print(indent: Int): String = {
    s"${indentString(indent)}$name"
  }

  def typeNamePairInC(): String = s"${BrboType.toCString(typ)} $name"

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

case class StringLiteral(value: String, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(STRING, uuid) with BrboValue {
  override def print(indent: Int): String = {
    s"${indentString(indent)}$value"
  }

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

case class Bool(b: Boolean, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL, uuid) with BrboValue {
  override def print(indent: Int): String = {
    s"${indentString(indent)}${b.toString}"
  }

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

case class Number(n: Int, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT, uuid) with BrboValue {
  override def print(indent: Int): String = {
    s"${indentString(indent)}${n.toString}"
  }


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

case class BrboArray(values: List[BrboExpr], innerType: BrboType.T, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(ARRAY(innerType), uuid) with BrboValue {
  assert(values.forall(v => v.isInstanceOf[BrboValue] && v.typ == innerType))

  override def print(indent: Int): String = s"Array(${values.map(v => v.print(0)).mkString(",")})"

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
}

case class Read(array: BrboExpr, index: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(array.typ.asInstanceOf[BrboType.ARRAY].typ, uuid) {
  override def print(indent: Int): String = ???

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
}

case class Length(array: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT, uuid) {
  assert(array.typ.isInstanceOf[BrboType.ARRAY])

  override def print(indent: Int): String = ???

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
}

case class Addition(left: BrboExpr, right: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT, uuid) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def print(indent: Int): String = {
    s"${indentString(indent)}(${left.print(0)} + ${right.print(0)})"
  }

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

case class Subtraction(left: BrboExpr, right: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT, uuid) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def print(indent: Int): String = {
    s"${indentString(indent)}(${left.print(0)} - ${right.print(0)})"
  }

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

case class Multiplication(left: BrboExpr, right: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(INT, uuid) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def print(indent: Int): String = {
    s"${indentString(indent)}(${left.print(0)} * ${right.print(0)})"
  }

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

case class Negation(expression: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL, uuid) {
  assert(expression.typ == BOOL)

  override def print(indent: Int): String = {
    s"${indentString(indent)}!(${expression.print(0)})"
  }

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

case class LessThan(left: BrboExpr, right: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL, uuid) {
  assert(left.typ == INT)
  assert(right.typ == INT)

  override def print(indent: Int): String = {
    s"${indentString(indent)}(${left.print(0)} < ${right.print(0)})"
  }

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

case class Equal(left: BrboExpr, right: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL, uuid) {
  assert(left.typ == right.typ)

  override def print(indent: Int): String = s"${indentString(indent)}(${left.print(0)} == ${right.print(0)})"

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

case class And(left: BrboExpr, right: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL, uuid) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def print(indent: Int): String = s"${indentString(indent)}(${left.print(0)} && ${right.print(0)})"

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

case class Or(left: BrboExpr, right: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(BOOL, uuid) {
  assert(left.typ == BOOL)
  assert(right.typ == BOOL)

  override def print(indent: Int): String = s"${indentString(indent)}(${left.print(0)} || ${right.print(0)})"


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

case class FunctionCallExpr(identifier: String, arguments: List[BrboExpr], returnType: BrboType.T, override val uuid: UUID = UUID.randomUUID())
  extends BrboExpr(returnType, uuid) {
  override def print(indent: Int): String = {
    val argumentsString = arguments.map(argument => argument.printNoOuterBrackets).mkString(", ")
    s"${indentString(indent)}$identifier($argumentsString)"
  }


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

case class ITEExpr(condition: BrboExpr, thenExpr: BrboExpr, elseExpr: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends BrboExpr(thenExpr.typ, uuid) {
  assert(condition.typ == BOOL)
  assert(thenExpr.typ == elseExpr.typ)

  override def print(indent: Int): String = s"${indentString(indent)}${condition.print(0)} ? ${thenExpr.print(0)} : ${elseExpr.print(0)}"


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
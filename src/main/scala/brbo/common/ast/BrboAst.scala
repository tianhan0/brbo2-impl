package brbo.common.ast

import brbo.common.BrboType._
import brbo.common.GhostVariableTyp._
import brbo.common.{BrboType, GhostVariableUtils, SameAs}

import java.util.UUID

@SerialVersionUID(1L)
case class BrboProgram(name: String, mainFunction: BrboFunction,
                       boundAssertions: List[BoundAssertion] = Nil,
                       functions: List[BrboFunction] = Nil, uuid: UUID = UUID.randomUUID())
  extends PrintToC with SameAs with Serializable {
  override def printToC(indent: Int): String = {
    val functionsString = (functions :+ mainFunction).map(function => function.printToC(indent)).mkString("\n")
    s"${PreDefinedFunctions.UNDEFINED_FUNCTIONS_MACRO}\n${PreDefinedFunctions.SYMBOLS_MACRO}\n$functionsString"
  }

  def toIR(indent: Int = 0): String = {
    val separator = ", "
    s"Program name: `$name`\n" +
      s"Global assertions to verify: `${boundAssertions.map(a => a.assertion.toString).mkString(separator)}`\n" +
      s"${(functions :+ mainFunction).map(function => function.printToC(DEFAULT_INDENT)).mkString("\n")}"
  }

  def replaceMainFunction(newMainFunction: BrboFunction): BrboProgram =
    BrboProgram(name, newMainFunction, boundAssertions, functions)

  override def sameAs(other: Any): Boolean = {
    other match {
      case BrboProgram(otherName, otherMainFunction, otherBoundAssertions, otherFunctions, _) =>
        if (otherBoundAssertions.length != boundAssertions.length || otherFunctions.length != functions.length)
          false
        else
          otherName == name && otherMainFunction.sameAs(mainFunction) &&
            otherBoundAssertions.zip(boundAssertions).forall({
              case (a1, a2) => a1.sameAs(a2)
            }) && otherFunctions.zip(functions).forall({
            case (f1, f2) => f1.sameAs(f2)
          })
      case _ => false
    }
  }
}

/**
 *
 * @param identifier           Function name
 * @param returnType           Return type
 * @param parameters           Function parameters
 * @param bodyNoInitialization Function body without initializing ghost variables
 * @param groupIds             IDs of amortizations groups that *may* be used in this function,
 *                             so that their ghost variables will be initialized.
 *                             This set is empty iff. no amortization (i.e., selectively, worst-case, or fully-amortized reasoning)
 *                             has been applied to the function.
 * @param uuid                 Unique ID
 */
@SerialVersionUID(2L)
case class BrboFunction(identifier: String, returnType: BrboType.T, parameters: List[Identifier],
                        bodyNoInitialization: Statement, groupIds: Set[Int], uuid: UUID = UUID.randomUUID())
  extends PrintToC with SameAs with Serializable {
  val ghostVariableInitializations: List[Command] = groupIds.flatMap({
    groupId => GhostVariableUtils.declareVariables(groupId)
  }).toList.sortWith({ case (c1, c2) => c1.printToIR() < c2.printToIR() })
  val approximatedResourceUsage: BrboExpr = GhostVariableUtils.approximatedResourceUsage(groupIds)

  // Declare and initialize ghost variables in the function
  val actualBody: Statement = bodyNoInitialization match {
    case Block(asts, _) => Block(ghostVariableInitializations ::: asts)
    case ITE(_, _, _, _) | Loop(_, _, _) => Block(ghostVariableInitializations :+ bodyNoInitialization)
    case _ => throw new Exception
  }

  override def printToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePairInC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) \n${actualBody.printToC(0)}"
  }

  val isAmortized: Boolean = groupIds.isEmpty

  def replaceBodyWithoutInitialization(newBody: Statement): BrboFunction = BrboFunction(identifier, returnType, parameters, newBody, groupIds)

  def replaceGroupIds(newGroupIds: Set[Int]): BrboFunction = BrboFunction(identifier, returnType, parameters, bodyNoInitialization, newGroupIds)

  def sameAs(other: Any): Boolean = {
    other match {
      case BrboFunction(otherIdentifier, otherReturnType, otherParameters, otherBodyNoInitialization, otherGroupIds, _) =>
        val sameParameters = {
          if (otherParameters.length != parameters.length) false
          else otherParameters.zip(parameters).forall({ case (i1, i2) => i1.sameAs(i2) })
        }
        otherIdentifier == identifier && otherReturnType == returnType && sameParameters &&
          otherBodyNoInitialization.sameAs(bodyNoInitialization) && otherGroupIds == groupIds
      case _ => false
    }
  }
}

abstract class BrboAst extends SameAs with Serializable with PrintToC

abstract class Statement(val uuid: UUID) extends BrboAst

abstract class Command(val uuid: UUID) extends BrboAst with PrintToIR with GetFunctionCalls with UseDefVariables {
  protected def printToCCommand(indent: Int): String

  final override def printToIR(): String = {
    this match {
      case brboExpr: BrboExpr => brboExpr.print(0)
      case BeforeFunctionCall(callee, actualArguments, _) =>
        val argumentsString =
          if (actualArguments.nonEmpty) s" with `${actualArguments.map(a => a.printToIR()).mkString(", ")}`"
          else " no arguments"
        s"Call function `${callee.identifier}`$argumentsString"
      case use@Use(_, update, condition, _) =>
        s"if (${condition.printNoOuterBrackets}) use ${use.resourceVariable.name} ${update.printToIR()}"
      case reset@Reset(_, condition, _) =>
        s"if (${condition.printNoOuterBrackets}) reset ${reset.resourceVariable.name}"
      case _: Command => printToC(0)
      case _ => throw new Exception()
    }
  }

  final override def printToC(indent: Int): String = {
    this match {
      case brboExpr: BrboExpr => brboExpr.print(indent) + ";"
      case _: Command => printToCCommand(indent)
      case _ => throw new Exception()
    }
  }
}

case class Block(asts: List[BrboAst], override val uuid: UUID = UUID.randomUUID()) extends Statement(uuid) {
  override def printToC(indent: Int): String = {
    s"${indentString(indent)}{\n${asts.map(t => t.printToC(indent + DEFAULT_INDENT)).mkString("\n")}\n${indentString(indent)}}"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Block(otherAsts, _) =>
        if (otherAsts.length != asts.length) false
        else otherAsts.zip(asts).forall({ case (s1, s2) => s1.sameAs(s2) })
      case _ => false
    }
  }
}

case class Loop(condition: BrboExpr, loopBody: BrboAst, override val uuid: UUID = UUID.randomUUID()) extends Statement(uuid) {
  override def printToC(indent: Int): String = {
    val conditionString = condition.printNoOuterBrackets
    val bodyString = loopBody match {
      case _: Command => s"${loopBody.printToC(indent + DEFAULT_INDENT)}"
      case _: Statement => s"${loopBody.printToC(indent)}"
      case _ => throw new Exception
    }
    s"${indentString(indent)}while ($conditionString)\n$bodyString"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Loop(otherCondition, otherLoopBody, _) =>
        otherCondition.sameAs(condition) && otherLoopBody.sameAs(loopBody)
      case _ => false
    }
  }
}

case class ITE(condition: BrboExpr, thenAst: BrboAst, elseAst: BrboAst, override val uuid: UUID = UUID.randomUUID()) extends Statement(uuid) {
  override def printToC(indent: Int): String = {
    val conditionString = condition.printNoOuterBrackets
    val thenString = thenAst match {
      case _: Command => thenAst.printToC(indent + DEFAULT_INDENT)
      case _: Statement => thenAst.printToC(indent)
      case _ => throw new Exception
    }
    val elseString = elseAst match {
      case _: Command => elseAst.printToC(indent + DEFAULT_INDENT)
      case _: Statement => elseAst.printToC(indent)
      case _ => throw new Exception
    }
    s"${indentString(indent)}if ($conditionString)\n$thenString\n${indentString(indent)}else\n$elseString"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case ITE(otherCondition, otherThenAst, otherElseAst, _) =>
        otherCondition.sameAs(condition) && otherThenAst.sameAs(thenAst) && otherElseAst.sameAs(elseAst)
      case _ => false
    }
  }
}

case class Assume(condition: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  assert(condition.typ == BOOL)

  override def printToCCommand(indent: Int): String = {
    val conditionString = condition.printNoOuterBrackets
    s"${indentString(indent)}assume($conditionString);"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = condition.getFunctionCalls

  override def getUses: Set[Identifier] = condition.getUses

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Assume(otherCondition, _) => condition.sameAs(otherCondition)
          case _ => false
        }
      case _ => false
    }
  }
}

case class VariableDeclaration(identifier: Identifier, initialValue: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCCommand(indent: Int): String = {
    val initialValueString =
      identifier.typ match {
        case INT => assert(initialValue.typ == INT, s"variable: `$identifier` (type: `${identifier.typ}`); initialValue: `$initialValue` (type: `${initialValue.typ}`)"); initialValue.printNoOuterBrackets
        case BOOL => assert(initialValue.typ == BOOL); initialValue.printNoOuterBrackets
        case STRING => assert(initialValue.typ == STRING); initialValue.printNoOuterBrackets
        case _ => throw new Exception
      }
    s"${indentString(indent)}${BrboType.toCString(identifier.typ)} ${identifier.name} = $initialValueString;"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = initialValue.getFunctionCalls

  override def getUses: Set[Identifier] = initialValue.getUses

  override def getDefs: Set[Identifier] = Set(identifier)

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case VariableDeclaration(otherIdentifier, otherInitialValue, _) =>
            otherIdentifier.sameAs(identifier) && otherInitialValue.sameAs(initialValue)
          case _ => false
        }
      case _ => false
    }
  }
}

case class Assignment(identifier: Identifier, expression: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  assert(identifier.typ == expression.typ)

  override def printToCCommand(indent: Int): String = {
    s"${indentString(indent)}${identifier.name} = ${expression.printNoOuterBrackets};"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = expression.getFunctionCalls

  override def getUses: Set[Identifier] = expression.getUses + identifier

  override def getDefs: Set[Identifier] = expression.getDefs

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Assignment(otherIdentifier, otherExpression, _) =>
            otherIdentifier.sameAs(identifier) && otherExpression.sameAs(expression)
          case _ => false
        }
      case _ => false
    }
  }
}

case class Skip(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCCommand(indent: Int): String = {
    s"${indentString(indent)};"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Skip(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class Return(value: Option[BrboExpr], override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCCommand(indent: Int): String = {

    val valueString =
      value match {
        case Some(value) => s" ${value.printNoOuterBrackets}"
        case None => ""
      }
    s"${indentString(indent)}return$valueString;"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = {
    value match {
      case Some(value2) => value2.getFunctionCalls
      case None => Nil
    }
  }

  override def getUses: Set[Identifier] = {
    value match {
      case Some(v) => v.getUses
      case None => Set()
    }
  }

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Return(otherValue, _) =>
            (otherValue, value) match {
              case (Some(otherValue), Some(value)) => otherValue.sameAs(value)
              case (None, None) => true
              case _ => false
            }
          case _ => false
        }
      case _ => false
    }
  }
}

case class Break(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCCommand(indent: Int): String = {
    s"${indentString(indent)}break;"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Break(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class Continue(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCCommand(indent: Int): String = {
    s"${indentString(indent)}continue;"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Continue(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class LabeledCommand(label: String, command: Command, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  assert(!command.isInstanceOf[LabeledCommand])
  assert(!command.isInstanceOf[GhostCommand], s"Not allow ghost commands to be labeled, due to the translation in BrboAstInC")

  override def printToCCommand(indent: Int): String = {
    s"${indentString(indent)}$label: ${command.printToC(0)}"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = command.getFunctionCalls

  override def getUses: Set[Identifier] = command.getUses

  override def getDefs: Set[Identifier] = command.getDefs

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case LabeledCommand(otherLabel, otherCommand, _) =>
            otherLabel == label && otherCommand.sameAs(command)
          case _ => false
        }
      case _ => false
    }
  }
}

sealed trait CFGOnly

case class FunctionExit(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCCommand(indent: Int): String = "[Function Exit]"


  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case FunctionExit(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class LoopExit(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCCommand(indent: Int): String = "[Loop Exit]"

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case LoopExit(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class Empty(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCCommand(indent: Int): String = "[Empty Node]"

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Empty(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class BranchingHead(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCCommand(indent: Int): String = "[Branch Head]"

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case BranchingHead(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

sealed trait GhostCommand {
  def replace(newGroupId: Int): GhostCommand
}

case class Use(groupId: Option[Int], update: BrboExpr, condition: BrboExpr = Bool(b = true), override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with GhostCommand {
  groupId match {
    case Some(value) => assert(value >= 0) // This command represents updating a resource variable for some amortization group
    case None => // This command represents updating the original resource variable
  }

  val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource)

  val assignmentCommand: Assignment = Assignment(resourceVariable, Addition(resourceVariable, update))

  override def getFunctionCalls: List[FunctionCallExpr] = update.getFunctionCalls

  override def printToCCommand(indent: Int): String = {
    s"${indentString(indent)}if (${condition.printNoOuterBrackets}) ${assignmentCommand.printToC(0)}"
  }

  override def replace(newGroupId: Int): Use = Use(Some(newGroupId), update, condition)

  override def getUses: Set[Identifier] = update.getUses ++ condition.getUses

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Use(otherGroupId, otherUpdate, otherCondition, _) =>
            otherGroupId == groupId && otherUpdate.sameAs(update) && otherCondition.sameAs(condition)
          case _ => false
        }
      case _ => false
    }
  }
}

case class Reset(groupId: Int, condition: BrboExpr = Bool(b = true), override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with GhostCommand {
  val (resourceVariable: Identifier, starVariable: Identifier, counterVariable: Identifier) =
    GhostVariableUtils.generateVariables(Some(groupId))

  val maxCommand: Assignment = {
    val iteExpr = ITEExpr(LessThan(starVariable, resourceVariable), resourceVariable, starVariable)
    Assignment(starVariable, iteExpr)
  }
  val maxComparison: LessThan = LessThan(starVariable, resourceVariable)
  val maxAssignment: Assignment = Assignment(starVariable, resourceVariable)
  val maxStatement: ITE = ITE(maxComparison, maxAssignment, Skip())
  val resetCommand: Assignment = Assignment(resourceVariable, Number(0))
  val counterCommand: Assignment = Assignment(counterVariable, Addition(counterVariable, Number(1)))

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def printToCCommand(indent: Int): String = {
    s"${indentString(indent)}if (${condition.printNoOuterBrackets}) {\n${maxStatement.printToC(indent + DEFAULT_INDENT)}\n${resetCommand.printToC(indent + DEFAULT_INDENT)}\n${counterCommand.printToC(indent + DEFAULT_INDENT)}\n${indentString(indent)}}"
  }

  def replace(newGroupId: Int): Reset = Reset(newGroupId, condition)

  override def getUses: Set[Identifier] = condition.getUses

  override def getDefs: Set[Identifier] = Set()

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Reset(otherGroupId, otherCondition, _) =>
            otherGroupId == groupId && otherCondition.sameAs(condition)
          case _ => false
        }
      case _ => false
    }
  }
}

sealed trait CexPathOnly

case class BeforeFunctionCall(callee: BrboFunction, actualArguments: List[BrboExpr], override val uuid: UUID = UUID.randomUUID())
  extends Command(uuid) with CexPathOnly {
  override def getFunctionCalls: List[FunctionCallExpr] = throw new Exception

  override def printToCCommand(indent: Int): String = throw new Exception

  override def getUses: Set[Identifier] = throw new Exception

  override def getDefs: Set[Identifier] = throw new Exception

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case BeforeFunctionCall(otherCallee, otherActualArguments, _) =>
            val sameArguements =
              if (otherActualArguments.length != actualArguments.length) false
              else otherActualArguments.zip(actualArguments).forall({ case (a1, a2) => a1.sameAs(a2) })
            callee.sameAs(otherCallee) && sameArguements
          case _ => false
        }
      case _ => false
    }
  }
}
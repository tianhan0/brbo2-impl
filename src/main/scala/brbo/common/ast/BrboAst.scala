package brbo.common.ast

import brbo.common.BrboType._
import brbo.common.GhostVariableTyp._
import brbo.common.{BrboType, GhostVariableUtils, SameAs}

import java.util.UUID

@SerialVersionUID(1L)
case class BrboProgram(name: String, mainFunction: BrboFunction,
                       boundAssertions: List[BoundAssertion] = Nil,
                       functions: List[BrboFunction] = Nil, uuid: UUID = UUID.randomUUID())
  extends PrettyPrintToC with OverrideToString with SameAs with Serializable {
  override def prettyPrintToC(indent: Int): String = {
    val functionsString = (functions :+ mainFunction).map(function => function.prettyPrintToC(indent)).mkString("\n")
    s"${PreDefinedFunctions.UNDEFINED_FUNCTIONS_MACRO}\n${PreDefinedFunctions.SYMBOLS_MACRO}\n$functionsString"
  }

  override def toIR(indent: Int = 0): String = {
    val separator = ", "
    s"Program name: `$name`\n" +
      s"Global assertions to verify: `${boundAssertions.map(a => a.assertion.toString).mkString(separator)}`\n" +
      s"${(functions :+ mainFunction).map(function => function.toIR(DEFAULT_INDENT_IR)).mkString("\n")}"
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
case class BrboFunction(identifier: String, returnType: BrboType, parameters: List[Identifier],
                        bodyNoInitialization: Statement, groupIds: Set[Int], uuid: UUID = UUID.randomUUID())
  extends PrettyPrintToC with OverrideToString with SameAs with Serializable {
  val ghostVariableInitializations: List[Command] = groupIds.flatMap({
    groupId => GhostVariableUtils.declareVariables(groupId)
  }).toList.sortWith({ case (c1, c2) => c1.toIR() < c2.toIR() })
  val approximatedResourceUsage: BrboExpr = GhostVariableUtils.approximatedResourceUsage(groupIds)

  // Declare and initialize ghost variables in the function
  val actualBody: Statement = bodyNoInitialization match {
    case Block(asts, _) => Block(ghostVariableInitializations ::: asts)
    case ITE(_, _, _, _) | Loop(_, _, _) => Block(ghostVariableInitializations :+ bodyNoInitialization)
    case _ => throw new Exception
  }

  override def prettyPrintToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePairInC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) \n${actualBody.prettyPrintToC(0)}"
  }

  def toIR(indent: Int = 0): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePairInC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) \n${actualBody.toIR(indent)}"
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

trait BrboAst extends BrboAstNode with PrettyPrintToC with OverrideToString

abstract class Command extends BrboAst with CommandOrExpr with GetFunctionCalls with UseDefVariables {
  override def toIR(indent: Int): String = prettyPrintToC(indent)
}

abstract class Statement extends BrboAst

case class Block(asts: List[BrboAst], uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString{\n${asts.map(t => t.prettyPrintToC(indent + DEFAULT_INDENT)).mkString("\n")}\n$indentString}"
  }

  override def toIR(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString{\n${asts.map(t => t.toIR(indent + DEFAULT_INDENT_IR)).mkString("\n")}\n$indentString}"
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

case class Loop(condition: BrboExpr, loopBody: BrboAst, uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val bodyString = loopBody match {
      case _: Command => s"${loopBody.prettyPrintToC(indent + DEFAULT_INDENT)}"
      case _: Statement => s"${loopBody.prettyPrintToC(indent)}"
      case _ => throw new Exception
    }
    val indentString = " " * indent
    s"${indentString}while ($conditionString)\n$bodyString"
  }

  override def toIR(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val bodyString = loopBody match {
      case _: Command => s"${loopBody.toIR(indent + DEFAULT_INDENT_IR)}"
      case _: Statement => s"${loopBody.toIR(indent)}"
      case _ => throw new Exception
    }
    val indentString = " " * indent
    s"${indentString}while ($conditionString)\n$bodyString"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Loop(otherCondition, otherLoopBody, _) =>
        otherCondition.sameAs(condition) && otherLoopBody.sameAs(loopBody)
      case _ => false
    }
  }
}

case class ITE(condition: BrboExpr, thenAst: BrboAst, elseAst: BrboAst, uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val thenString = thenAst match {
      case _: Command => thenAst.prettyPrintToC(indent + DEFAULT_INDENT)
      case _: Statement => thenAst.prettyPrintToC(indent)
      case _ => throw new Exception
    }
    val elseString = elseAst match {
      case _: Command => elseAst.prettyPrintToC(indent + DEFAULT_INDENT)
      case _: Statement => elseAst.prettyPrintToC(indent)
      case _ => throw new Exception
    }
    val indentString = " " * indent
    s"${indentString}if ($conditionString)\n$thenString\n${indentString}else\n$elseString"
  }

  override def toIR(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val thenString = thenAst match {
      case _: Command => thenAst.toIR(indent + DEFAULT_INDENT_IR)
      case _: Statement => thenAst.toIR(indent)
      case _ => throw new Exception
    }
    val elseString = elseAst match {
      case _: Command => elseAst.toIR(indent + DEFAULT_INDENT_IR)
      case _: Statement => elseAst.toIR(indent)
      case _ => throw new Exception
    }
    val indentString = " " * indent
    s"${indentString}if ($conditionString)\n$thenString\n${indentString}else\n$elseString"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case ITE(otherCondition, otherThenAst, otherElseAst, _) =>
        otherCondition.sameAs(condition) && otherThenAst.sameAs(thenAst) && otherElseAst.sameAs(elseAst)
      case _ => false
    }
  }
}

/*case class Assert(condition: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val indentString = " " * indent
    s"${indentString}assert($conditionString);"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = condition.getFunctionCalls
}*/

case class Assume(condition: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val indentString = " " * indent
    s"${indentString}assume($conditionString);"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class VariableDeclaration(identifier: Identifier, initialValue: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val initialValueString =
      identifier.typ match {
        case INT => assert(initialValue.typ == INT, s"variable: `$identifier` (type: `${identifier.typ}`); initialValue: `$initialValue` (type: `${initialValue.typ}`)"); initialValue.prettyPrintToCNoOuterBrackets
        case BOOL => assert(initialValue.typ == BOOL); initialValue.prettyPrintToCNoOuterBrackets
        case STRING => assert(initialValue.typ == STRING); initialValue.prettyPrintToCNoOuterBrackets
        case VOID => throw new Exception
      }
    val indentString = " " * indent
    s"$indentString${BrboType.toCString(identifier.typ)} ${identifier.name} = $initialValueString;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class Assignment(identifier: Identifier, expression: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(identifier.typ == expression.typ)

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${identifier.name} = ${expression.prettyPrintToCNoOuterBrackets};"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class FunctionCall(functionCallExpr: FunctionCallExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${functionCallExpr.prettyPrintToC()};"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = functionCallExpr.getFunctionCalls

  override def getUses: Set[Identifier] = functionCallExpr.getUses

  override def getDefs: Set[Identifier] = functionCallExpr.getDefs

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case FunctionCall(otherFunctionCallExpr, _) => otherFunctionCallExpr.sameAs(functionCallExpr)
          case _ => false
        }
      case _ => false
    }
  }
}

case class Skip(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class Return(value: Option[BrboExpr], uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    val valueString =
      value match {
        case Some(value) => s" ${value.prettyPrintToCNoOuterBrackets}"
        case None => ""
      }
    s"${indentString}return$valueString;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class Break(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}break;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class Continue(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}continue;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class LabeledCommand(label: String, command: Command, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(!command.isInstanceOf[LabeledCommand])
  assert(!command.isInstanceOf[GhostCommand], s"Not allow ghost commands to be labeled, due to the translation in BrboAstInC")

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString$label: ${command.prettyPrintToC()}"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class FunctionExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Function Exit]"

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class LoopExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Loop Exit]"

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class Empty(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Empty Node]"

  override def prettyPrintToCFG: String = prettyPrintToC()

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

case class BranchingHead(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Branch Head]"

  override def prettyPrintToCFG: String = prettyPrintToC()

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

/*case class UndefinedFunction(functionName: String, uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = s"[Undefined Function: `$functionName`]"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}*/

sealed trait GhostCommand {
  def replace(newGroupId: Int): GhostCommand
}

case class Use(groupId: Option[Int], update: BrboExpr, condition: BrboExpr = Bool(b = true), uuid: UUID = UUID.randomUUID()) extends Command with GhostCommand {
  groupId match {
    case Some(value) => assert(value >= 0) // This command represents updating a resource variable for some amortization group
    case None => // This command represents updating the original resource variable
  }

  val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource)

  val assignmentCommand: Assignment = Assignment(resourceVariable, Addition(resourceVariable, update))

  override def prettyPrintToCFG: String = {
    s"if (${condition.prettyPrintToCNoOuterBrackets}) use ${resourceVariable.name} ${update.prettyPrintToCFG}"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = update.getFunctionCalls

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}if (${condition.prettyPrintToCNoOuterBrackets}) ${assignmentCommand.prettyPrintToC()}"
  }

  override def toIR(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}if (${condition.prettyPrintToCNoOuterBrackets}) use ${resourceVariable.name} ${update.prettyPrintToCFG}"
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

case class Reset(groupId: Int, condition: BrboExpr = Bool(b = true), uuid: UUID = UUID.randomUUID()) extends Command with GhostCommand {
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

  override def prettyPrintToCFG: String = s"if (${condition.prettyPrintToCNoOuterBrackets}) reset ${resourceVariable.name}"

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}if (${condition.prettyPrintToCNoOuterBrackets}) {\n${maxStatement.prettyPrintToC(indent + DEFAULT_INDENT)}\n${resetCommand.prettyPrintToC(indent + DEFAULT_INDENT)}\n${counterCommand.prettyPrintToC(indent + DEFAULT_INDENT)}\n$indentString}"
  }

  override def toIR(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}if (${condition.prettyPrintToCNoOuterBrackets}) reset ${resourceVariable.name}"
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

case class BeforeFunctionCall(callee: BrboFunction, actualArguments: List[BrboExpr], uuid: UUID = UUID.randomUUID()) extends Command with CexPathOnly {
  override def prettyPrintToCFG: String = {
    val argumentsString =
      if (actualArguments.nonEmpty) s" with `${actualArguments.map(a => a.prettyPrintToCFG).mkString(", ")}`"
      else " no arguments"
    s"Call function `${callee.identifier}`$argumentsString"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = throw new Exception

  override def prettyPrintToC(indent: Int): String = throw new Exception

  override def getUses: Set[Identifier] = throw new Exception

  override def getDefs: Set[Identifier] = throw new Exception

  override def toIR(indent: Int): String = prettyPrintToCFG

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
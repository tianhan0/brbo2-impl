package brbo.common.ast

import brbo.common.BrboType._
import brbo.common.GhostVariableTyp._
import brbo.common.{BrboType, GhostVariableUtils}

import java.util.UUID

case class BrboProgram(name: String, mainFunction: BrboFunction, groupIds: Set[Int],
                       mostPreciseAssertion: Option[BrboExpr] = None, lessPreciseAssertion: Option[BrboExpr] = None,
                       functions: List[BrboFunction] = Nil, uuid: UUID = UUID.randomUUID()) extends PrettyPrintToC {
  override def prettyPrintToC(indent: Int): String = {
    // TODO: Declare and initialize ghost variables in the main function
    val functionsString = (functions :+ mainFunction).map(function => function.prettyPrintToC(indent)).mkString("\n")
    s"${PreDefinedFunctions.UNDEFINED_FUNCTIONS_MACRO}\n${PreDefinedFunctions.SYMBOLS_MACRO}\n$functionsString"
  }

  override def toString: String = {
    s"Program name: `$name`\n" +
      s"Group IDs: `$groupIds`\n" +
      s"Most precise bound: `$mostPreciseAssertion`\n" +
      s"Less precise bound: `$lessPreciseAssertion`\n" +
      s"${(functions :+ mainFunction).map(function => function.prettyPrintToC(DEFAULT_INDENT)).mkString("\n")}"
  }
}

case class BrboFunction(identifier: String, returnType: BrboType, parameters: List[Identifier], body: Statement, uuid: UUID = UUID.randomUUID()) extends PrettyPrintToC {
  override def prettyPrintToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePairInC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) \n${body.prettyPrintToC(0)}"
  }

  override def toString: String = prettyPrintToC()
}

abstract class BrboAst extends PrettyPrintToC {
  override def toString: String = prettyPrintToC()
}

abstract class Command extends BrboAst with PrettyPrintToCFG with GetFunctionCalls

abstract class Statement extends BrboAst

case class Block(asts: List[BrboAst], uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString{\n${asts.map(t => t.prettyPrintToC(indent + DEFAULT_INDENT)).mkString("\n")}\n$indentString}"
  }
}

case class Loop(condition: BrboExpr, body: BrboAst, uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val bodyString = body match {
      case _: Command => s"${body.prettyPrintToC(indent + DEFAULT_INDENT)}"
      case _: Statement => s"${body.prettyPrintToC(indent)}"
      case _ => throw new Exception
    }
    val indentString = " " * indent
    s"${indentString}while ($conditionString)\n$bodyString"
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
}

case class Assume(condition: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val indentString = " " * indent
    s"${indentString}assume($conditionString);"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = condition.getFunctionCalls
}*/

case class VariableDeclaration(variable: Identifier, initialValue: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val initialValueString =
      variable.typ match {
        case INT => assert(initialValue.typ == INT); initialValue.prettyPrintToCNoOuterBrackets
        case BOOL => assert(initialValue.typ == BOOL); initialValue.prettyPrintToCNoOuterBrackets
        case VOID => throw new Exception
      }
    val indentString = " " * indent
    s"$indentString${BrboType.toCString(variable.typ)} ${variable.identifier} = $initialValueString;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = initialValue.getFunctionCalls
}

case class Assignment(variable: Identifier, expression: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(variable.typ == expression.typ)

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${variable.identifier} = ${expression.prettyPrintToCNoOuterBrackets};"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = expression.getFunctionCalls
}

case class FunctionCall(functionCallExpr: FunctionCallExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${functionCallExpr.prettyPrintToC()};"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = functionCallExpr.getFunctionCalls
}

case class Skip(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
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
}

case class Break(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}break;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class Continue(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}continue;"
  }

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
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
}

sealed trait CFGOnly

case class FunctionExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Function Exit]"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class LoopExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Loop Exit]"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class Empty(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Empty node]"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

/*case class UndefinedFunction(functionName: String, uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = s"[Undefined Function: `$functionName`]"

  override def prettyPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}*/

sealed trait GhostCommand

case class Use(groupId: Option[Int], update: BrboExpr, condition: BrboExpr = Bool(b = true), uuid: UUID = UUID.randomUUID()) extends Command with GhostCommand {
  groupId match {
    case Some(value) => assert(value >= 0) // This command represents updating a resource variable for some amortization group
    case None => // This command represents updating the original resource variable
  }

  // TODO: Check this use command corresponds to the assignment command
  val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource)

  val assignmentCommand: Assignment = Assignment(resourceVariable, Addition(resourceVariable, update))

  override def prettyPrintToCFG: String = {
    s"if (${condition.prettyPrintToCNoOuterBrackets}) use ${resourceVariable.identifier} ${update.prettyPrintToCFG}"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = update.getFunctionCalls

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}if (${condition.prettyPrintToCNoOuterBrackets}) ${assignmentCommand.prettyPrintToC()}"
  }
}

case class Reset(groupId: Int, condition: BrboExpr = Bool(b = true), uuid: UUID = UUID.randomUUID()) extends Command with GhostCommand {
  val sharpVariable: Identifier = GhostVariableUtils.generateVariable(Some(groupId), Sharp)
  val resourceVariable: Identifier = GhostVariableUtils.generateVariable(Some(groupId), Resource)
  val counterVariable: Identifier = GhostVariableUtils.generateVariable(Some(groupId), Counter)

  val maxCommand: Assignment = {
    val iteExpr = ITEExpr(LessThan(sharpVariable, resourceVariable), resourceVariable, sharpVariable)
    Assignment(sharpVariable, iteExpr)
  }
  val maxComparison: LessThan = LessThan(sharpVariable, resourceVariable)
  val maxAssignment: Assignment = Assignment(sharpVariable, resourceVariable)
  val maxStatement: ITE = ITE(maxComparison, maxAssignment, Skip())
  val resetCommand: Assignment = Assignment(resourceVariable, Number(0))
  val counterCommand: Assignment = Assignment(counterVariable, Addition(counterVariable, Number(1)))

  override def prettyPrintToCFG: String = s"if (${condition.prettyPrintToCNoOuterBrackets}) reset ${resourceVariable.identifier}"

  override def getFunctionCalls: List[FunctionCallExpr] = Nil

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}if (${condition.prettyPrintToCNoOuterBrackets}) {\n${maxStatement.prettyPrintToC(indent + DEFAULT_INDENT)}\n${resetCommand.prettyPrintToC(indent + DEFAULT_INDENT)}\n${counterCommand.prettyPrintToC(indent + DEFAULT_INDENT)}\n$indentString}"
  }
}

sealed trait CexPathOnly

case class CallFunction(callee: BrboFunction, actualArguments: List[BrboExpr]) extends Command with CexPathOnly {
  override def prettyPrintToCFG: String = {
    val argumentsString =
      if (actualArguments.nonEmpty) s" with `${actualArguments.map(a => a.prettyPrintToCFG).mkString(", ")}`"
      else " no arguments"
    s"Call function `${callee.identifier}`$argumentsString"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = ???

  override def prettyPrintToC(indent: Int): String = ???
}
package brbo.common.ast

import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT, VOID}

import java.util.UUID

case class BrboProgram(name: String, mainFunction: BrboFunction, functions: List[BrboFunction] = Nil, uuid: UUID = UUID.randomUUID()) extends PrettyPrintToC {
  override def prettyPrintToC(indent: Int): String = {
    val functionsString = (functions :+ mainFunction).map(function => function.prettyPrintToC(indent)).mkString("\n")
    s"${PreDefinedBrboFunctions.UNDEFINED_FUNCTIONS_MACRO}\n${PreDefinedBrboFunctions.SYMBOLS_MACRO}\n$functionsString"
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

abstract class Command extends BrboAst with PrettyPrintPrintToCFG with GetFunctionCalls

abstract class Statement extends BrboAst

case class Block(statements: List[BrboAst], uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString{\n${statements.map(t => t.prettyPrintToC(indent + DEFAULT_INDENT)).mkString("\n")}\n$indentString}"
  }
}

case class Loop(condition: BrboExpr, body: BrboAst, uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val bodyString = s"${body.prettyPrintToC(indent)}"
    val indentString = " " * indent
    s"${indentString}while ($conditionString)\n$bodyString"
  }
}

case class ITE(condition: BrboExpr, thenAst: BrboAst, elseAst: BrboAst, uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToCNoOuterBrackets
    val thenString = thenAst.prettyPrintToC(indent + DEFAULT_INDENT)
    val elseString = elseAst.prettyPrintToC(indent + DEFAULT_INDENT)
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

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = initialValue.getFunctionCalls
}

case class Assignment(variable: Identifier, expression: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(variable.typ == expression.typ)

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${variable.identifier} = ${expression.prettyPrintToCNoOuterBrackets};"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = expression.getFunctionCalls
}

case class FunctionCall(identifier: Option[Identifier], functionCallExpr: FunctionCallExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    identifier match {
      case Some(value) => s"$indentString${value.prettyPrintToC()} = ${functionCallExpr.prettyPrintToC()};"
      case None => s"$indentString${functionCallExpr.prettyPrintToC()};"
    }
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = functionCallExpr.getFunctionCalls
}

case class Skip(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

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

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

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

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class Continue(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}continue;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class LabeledCommand(label: String, command: Command, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(!command.isInstanceOf[LabeledCommand])

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString$label: ${command.prettyPrintToC()}"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = command.getFunctionCalls
}

sealed trait CFGOnly

case class FunctionExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Function Exit]"

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class LoopExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = "[Loop Exit]"

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class UndefinedFunction(functionName: String, uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = s"[Undefined Function: `$functionName`]"

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}
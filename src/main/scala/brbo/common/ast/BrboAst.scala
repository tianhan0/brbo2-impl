package brbo.common.ast

import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT, VOID}

import java.util.UUID

case class BrboProgram(name: String, mainFunction: BrboFunction, functions: List[BrboFunction] = Nil, uuid: UUID = UUID.randomUUID()) extends PrettyPrintToC {
  override def prettyPrintToC(indent: Int): String = {
    val macroes =
      """extern void __VERIFIER_error() __attribute__((noreturn));
        |extern void __VERIFIER_assume (int);
        |extern int __VERIFIER_nondet_int ();
        |#define static_assert __VERIFIER_assert
        |#define assume __VERIFIER_assume
        |#define LARGE_INT 1000000
        |#define true 1
        |#define false 0
        |#define boolean int
        |#define MAX 8
        |""".stripMargin

    val functionsString = (functions :+ mainFunction).map(function => function.prettyPrintToC(indent)).mkString("\n")
    s"$macroes\n\n$functionsString"
  }
}

case class BrboFunction(identifier: String, returnType: BrboType, parameters: List[Identifier], body: Statement, uuid: UUID = UUID.randomUUID()) extends PrettyPrintToC {
  override def prettyPrintToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePairInC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) {\n${body.prettyPrintToC(DEFAULT_INDENT)}\n}"
  }
}

abstract class BrboAst extends PrettyPrintToC

abstract class Command extends BrboAst with PrettyPrintPrintToCFG

abstract class Statement extends BrboAst

case class Block(statements: List[BrboAst], uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString{\n${statements.map(t => t.prettyPrintToC(indent + DEFAULT_INDENT)).mkString("\n")}\n$indentString}"
  }

}

case class Loop(condition: BrboExpr, body: BrboAst, uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val bodyString = s"${body.prettyPrintToC(indent)}"
    val indentString = " " * indent
    s"${indentString}while $conditionString\n$bodyString"
  }

}

case class ITE(condition: BrboExpr, thenAst: BrboAst, elseAst: BrboAst, uuid: UUID = UUID.randomUUID()) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val thenString = thenAst.prettyPrintToC(indent + DEFAULT_INDENT)
    val elseString = elseAst.prettyPrintToC(indent + DEFAULT_INDENT)
    val indentString = " " * indent
    s"${indentString}if($conditionString)\n$thenString\n${indentString}else\n$elseString"
  }

}

case class Assert(condition: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val indentString = " " * indent
    s"${indentString}assert($conditionString);"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class Assume(condition: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val indentString = " " * indent
    s"${indentString}assume($conditionString);"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class VariableDeclaration(variable: Identifier, initialValue: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val initialValueString =
      variable.typ match {
        case INT => assert(initialValue.typ == INT); initialValue.prettyPrintToC()
        case BOOL => assert(initialValue.typ == BOOL); initialValue.prettyPrintToC()
        case VOID => throw new Exception
      }
    val indentString = " " * indent
    s"$indentString${BrboType.toCString(variable.typ)} ${variable.identifier} = $initialValueString;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class Assignment(variable: Identifier, expression: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(variable.typ == expression.typ)

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${variable.identifier} = ${expression.prettyPrintToC()};"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

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

}

case class Skip(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class ReturnExpr(value: BrboExpr, uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}return ${value.prettyPrintToC()};"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class ReturnVoid(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}return;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class Break(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}break;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class Continue(uuid: UUID = UUID.randomUUID()) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}continue;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

case class LabeledCommand(label: String, command: Command, uuid: UUID = UUID.randomUUID()) extends Command {
  assert(!command.isInstanceOf[LabeledCommand])

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString$label: ${command.prettyPrintToC()}"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()

}

sealed trait CFGOnly

case class FunctionExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = throw new Exception("FunctionExit only exists in Control Flow Graphs")

  override def prettyPrintPrintToCFG: String = "[Function Exit]"

}

case class LoopExit(uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = throw new Exception("LoopExit only exists in Control Flow Graphs")

  override def prettyPrintPrintToCFG: String = "[Loop Exit]"

}

case class UndefinedFunction(functionName: String, uuid: UUID = UUID.randomUUID()) extends Command with CFGOnly {
  override def prettyPrintToC(indent: Int): String = throw new Exception("UndefinedFunction only exists in Control Flow Graphs")

  override def prettyPrintPrintToCFG: String = s"[Undefined Function: `$functionName`]"

}
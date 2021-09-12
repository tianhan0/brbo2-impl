package brbo.common.ast

import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT, VOID}
import brbo.common.UniqueID

import java.util.UUID

case class BrboProgram(mainFunction: BrboFunction, functions: List[BrboFunction] = Nil) extends UniqueID(UUID.randomUUID()) with PrettyPrintToC {
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

case class BrboFunction(identifier: String, returnType: BrboType, parameters: List[Identifier], body: Statement) extends UniqueID(UUID.randomUUID()) with PrettyPrintToC {
  override def prettyPrintToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePairInC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) {\n${body.prettyPrintToC(DEFAULT_INDENT)}\n}"
  }
}

abstract class BrboAst extends UniqueID(UUID.randomUUID()) with PrettyPrintToC

abstract class Command extends BrboAst with PrettyPrintPrintToCFG

abstract class Statement extends BrboAst

case class Block(statements: List[BrboAst]) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString{\n${statements.map(t => t.prettyPrintToC(indent + DEFAULT_INDENT)).mkString("\n")}\n$indentString}"
  }
}

case class Loop(condition: BrboExpr, body: BrboAst) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val bodyString = s"${body.prettyPrintToC(indent)}"
    val indentString = " " * indent
    s"${indentString}while $conditionString\n$bodyString"
  }
}

case class ITE(condition: BrboExpr, thenAst: BrboAst, elseAst: BrboAst) extends Statement {
  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val thenString = thenAst.prettyPrintToC(indent + DEFAULT_INDENT)
    val elseString = elseAst.prettyPrintToC(indent + DEFAULT_INDENT)
    val indentString = " " * indent
    s"${indentString}if($conditionString)\n$thenString\n${indentString}else\n$elseString"
  }
}

case class Assert(condition: BrboExpr) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val indentString = " " * indent
    s"${indentString}assert($conditionString);"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class Assume(condition: BrboExpr) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val indentString = " " * indent
    s"${indentString}assume($conditionString);"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class VariableDeclaration(variable: String, typ: BrboType, initialValue: BrboExpr) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val initialValueString =
      typ match {
        case INT => assert(initialValue.typ == INT); initialValue.prettyPrintToC()
        case BOOL => assert(initialValue.typ == BOOL); initialValue.prettyPrintToC()
        case VOID => throw new Exception
      }
    val indentString = " " * indent
    s"$indentString${BrboType.toCString(typ)} $variable = $initialValueString;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class Assignment(variable: Identifier, expression: BrboExpr) extends Command {
  assert(variable.typ == expression.typ)

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${variable.identifier} = ${expression.prettyPrintToC()};"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class FunctionCall(identifier: Option[Identifier], functionCallExpr: FunctionCallExpr) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    identifier match {
      case Some(value) => s"$indentString${value.prettyPrintToC()} = ${functionCallExpr.prettyPrintToC()};"
      case None => s"$indentString${functionCallExpr.prettyPrintToC()};"
    }
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class ReturnExpr(value: BrboExpr) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}return ${value.prettyPrintToC()};"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class ReturnVoid() extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}return;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class Skip() extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class Break() extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}break;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class Continue() extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}continue;"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}

case class FunctionExit() extends Command {
  override def prettyPrintToC(indent: Int): String = throw new Exception("FunctionExit only exists in Control Flow Graphs")

  override def prettyPrintPrintToCFG: String = "[Function Exit]"
}

case class LoopExit() extends Command {
  override def prettyPrintToC(indent: Int): String = throw new Exception("LoopExit only exists in Control Flow Graphs")

  override def prettyPrintPrintToCFG: String = "[Loop Exit]"
}

case class LabeledCommand(label: String, command: Command) extends Command {
  assert(!command.isInstanceOf[LabeledCommand])

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString$label: ${command.prettyPrintToC()}"
  }

  override def prettyPrintPrintToCFG: String = prettyPrintToC()
}
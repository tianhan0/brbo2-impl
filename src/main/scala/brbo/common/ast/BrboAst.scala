package brbo.common.ast

import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT, VOID}

import scala.collection.immutable.HashSet

sealed trait PrettyPrintToC {
  val DEFAULT_INDENT = 2

  def prettyPrintToC(indent: Int): String
}

case class BrboProgram(mainFunction: BrboFunction, functions: List[BrboFunction] = Nil) extends PrettyPrintToC {
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
        |void __VERIFIER_assert(int cond) {
        |  if (!(cond)) {
        |    ERROR: __VERIFIER_error();
        |  }
        |  return;
        |}
        |void assert(int cond) {
        |  if (!(cond)) {
        |    ERROR: __VERIFIER_error();
        |  }
        |  return;
        |}
        |int ndInt() {
        |  return __VERIFIER_nondet_int();
        |}
        |int ndBool() {
        |  int x = ndInt();
        |  assume(x == 1 || x == 0);
        |  return x;
        |}
        |int ndInt2(int lower, int upper) {
        |  int x = ndInt();
        |  assume(lower <= x && x <= upper);
        |  return x;
        |}""".stripMargin

    val functionsString = (mainFunction :: functions).map(function => function.prettyPrintToC(indent)).mkString("\n")
    s"$macroes\n\n$functionsString"
  }
}

case class BrboFunction(identifier: String, returnType: BrboType, parameters: List[Identifier], body: Statement) extends PrettyPrintToC {
  override def prettyPrintToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePairInC()}").mkString(", ")
    s"${BrboType.toCString(returnType)} $identifier($parametersString) {\n${body.prettyPrintToC(DEFAULT_INDENT)}\n}"
  }
}

sealed trait BrboAst extends PrettyPrintToC

sealed trait Command extends BrboAst

sealed trait Statement extends BrboAst

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
}

case class Assume(condition: BrboExpr) extends Command {
  assert(condition.typ == BOOL)

  override def prettyPrintToC(indent: Int): String = {
    val conditionString = condition.prettyPrintToC()
    val indentString = " " * indent
    s"${indentString}assume($conditionString);"
  }
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
}

case class Assignment(variable: Identifier, expression: BrboExpr) extends Command {
  assert(variable.typ == expression.typ)

  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${variable.identifier} = ${expression.prettyPrintToC()};"
  }
}

case class FunctionCall(functionCallExpr: FunctionCallExpr) extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString${functionCallExpr.prettyPrintToC()};"
  }
}

case object Return extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}return;"
  }
}

case object Skip extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"$indentString;"
  }
}

case object Break extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}break;"
  }
}

case object Continue extends Command {
  override def prettyPrintToC(indent: Int): String = {
    val indentString = " " * indent
    s"${indentString}continue;"
  }
}

object BrboAst {
  def getAllExprs(ast: BrboAst): Set[BrboExpr] = {
    ast match {
      case _: Command => new HashSet[BrboExpr]
      case statement: Statement =>
        statement match {
          case Block(statements) => statements.flatMap(statement => getAllExprs(statement)).toSet
          case Loop(_, body) => getAllExprs(body)
          case ITE(_, thenAst, elseAst) => getAllExprs(thenAst) ++ getAllExprs(elseAst)
        }
    }
  }

  def getAllCommands(ast: BrboAst): Set[Command] = {
    ast match {
      case command: Command => HashSet[Command](command)
      case statement: Statement =>
        statement match {
          case Block(statements) => statements.flatMap(statement => getAllCommands(statement)).toSet
          case Loop(_, body) => getAllCommands(body)
          case ITE(_, thenAst, elseAst) => getAllCommands(thenAst) ++ getAllCommands(elseAst)
        }
    }
  }
}
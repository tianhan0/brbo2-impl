package brbo.common.ast

import brbo.{StringCompare, TestCase}
import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.INT
import org.scalatest.flatspec.AnyFlatSpec

class BrboAstUnitTest extends AnyFlatSpec {
  BrboAstUnitTest.prettyPrintToCUnitTest.foreach({
    testCase =>
      StringCompare.compareLiteral(testCase.input.asInstanceOf[BrboAst].prettyPrintToC(2), testCase.expectedOutput, s"${testCase.name} failed!")
  })
}

object BrboAstUnitTest {
  val prettyPrintToCUnitTest: List[TestCase] =
    List[TestCase](
      TestCase("Continue", Continue(), "  continue;"),
      TestCase("Break", Break(), "  break;"),
      TestCase("Skip", Skip(), "  ;"),
      TestCase("ReturnExpr", ReturnExpr(Identifier("x", INT)), "  return x;"),
      TestCase("ReturnVoid", ReturnVoid(), "  return;"),
      TestCase("FunctionCall", FunctionCall(Some(Identifier("x", INT)), FunctionCallExpr("f", List("a", "b"), BrboType.INT)), "  x = f(a, b);"),
      TestCase("Assignment", Assignment(Identifier("x", INT), Number(0)), "  x = 0;"),
      TestCase("VariableDeclaration", VariableDeclaration("x", BrboType.INT, Number(1)), "  int x = 1;"),
      TestCase("Assert", Assert(Bool(false)), "  assert(false);"),
      TestCase("Assume", Assume(Bool(false)), "  assume(false);"),
      TestCase("LabeledCommand", LabeledCommand("label", ReturnVoid()), "  label: return;"),
      TestCase("ITE", ITE(Bool(true), Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1))), "  if(true)\n    x = 0;\n  else\n    x = 1;"),
      TestCase("Loop", Loop(LessThan(Number(0), Identifier("x", BrboType.INT)), Block(List(Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1))))), "  while (0 < x)\n  {\n    x = 0;\n    x = 1;\n  }"),
      TestCase("Block", Block(List(Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1)))), "  {\n    x = 0;\n    x = 1;\n  }")
    )
}

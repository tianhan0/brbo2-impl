package brbo.common.ast

import brbo.StringCompare
import brbo.common.TypeUtils.BrboType
import org.scalatest.flatspec.AnyFlatSpec

class BrboAstUnitTest extends AnyFlatSpec {
  BrboAstUnitTest.prettyPrintToCUnitTest.foreach({
    testCase =>
      StringCompare.compareLiteral(testCase.input.prettyPrintToC(2), testCase.expectedOutput, s"${testCase.name} failed!")
  })
}

object BrboAstUnitTest {
  val prettyPrintToCUnitTest: List[BrboAstTestCase] =
    List[BrboAstTestCase](
      BrboAstTestCase("Continue", Continue, "  continue;"),
      BrboAstTestCase("Break", Break, "  break;"),
      BrboAstTestCase("Skip", Skip, "  ;"),
      BrboAstTestCase("Return", Return, "  return;"),
      BrboAstTestCase("FunctionCall", FunctionCall(FunctionCallExpr("f", List("a", "b"), BrboType.INT)), "  f(a, b);"),
      BrboAstTestCase("Assignment", Assignment("x", Number(0)), "  x = 0;"),
      BrboAstTestCase("VariableDeclaration", VariableDeclaration("x", BrboType.INT, Number(1)), "  int x = 1;"),
      BrboAstTestCase("Assert", Assert(Bool(false)), "  assert(false);"),
      BrboAstTestCase("ITE", ITE(Bool(true), Assignment("x", Number(0)), Assignment("x", Number(1))), "  if(true)\n    x = 0;\n  else\n    x = 1;"),
      BrboAstTestCase("Loop", Loop(LessThan(Number(0), Identifier("x", BrboType.INT)), Block(List(Assignment("x", Number(0)), Assignment("x", Number(1))))), "  while (0 < x)\n  {\n    x = 0;\n    x = 1;\n  }"),
      BrboAstTestCase("Block", Block(List(Assignment("x", Number(0)), Assignment("x", Number(1)))), "  {\n    x = 0;\n    x = 1;\n  }")
    )
}

case class BrboAstTestCase(name: String, input: BrboAst, expectedOutput: String)
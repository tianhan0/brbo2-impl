package brbo.common.ast

import brbo.TestCase
import brbo.common.StringCompare
import brbo.common.BrboType
import brbo.common.BrboType.INT
import org.scalatest.flatspec.AnyFlatSpec

class BrboExprUnitTest extends AnyFlatSpec {
  "Pretty-printing BrboExpr to C expression" should "be correct" in {
    BrboExprUnitTest.prettyPrintToCUnitTest.foreach({
      testCase =>
        val expr = testCase.input.asInstanceOf[BrboExpr]
        StringCompare.compareLiteral(expr.prettyPrintToC(), testCase.expectedOutput, s"${testCase.name} failed!")
        BrboExprUtils.visit(expr) // Ensure every expression is handled by this pattern matching
    })
  }
}

object BrboExprUnitTest {
  val prettyPrintToCUnitTest: List[TestCase] =
    List[TestCase](
      TestCase("Identifier", Identifier("z", BrboType.INT), "z"),
      TestCase("Boolean", Bool(true), "true"),
      TestCase("Number", Number(23), "23"),
      TestCase("StringLiteral", StringLiteral("helloworld"), "helloworld"),
      TestCase("Addition", Addition(Number(2), Number(3)), "(2 + 3)"),
      TestCase("Subtraction", Subtraction(Number(2), Number(3)), "(2 - 3)"),
      TestCase("Multiplication", Multiplication(Number(2), Number(3)), "(2 * 3)"),
      TestCase("Division", Division(Number(2), Number(3)), "(2 / 3)"),
      TestCase("Negative", Negative(Bool(true)), "!(true)"),
      TestCase("LessThan", LessThan(Number(2), Number(3)), "(2 < 3)"),
      TestCase("LessThanOrEqualTo", LessThanOrEqualTo(Number(2), Number(3)), "(2 <= 3)"),
      TestCase("GreaterThan", GreaterThan(Number(2), Number(3)), "(2 > 3)"),
      TestCase("GreaterThanOrEqualTo", GreaterThanOrEqualTo(Number(2), Number(3)), "(2 >= 3)"),
      TestCase("Equal", Equal(Number(2), Number(3)), "(2 == 3)"),
      TestCase("NotEqual", NotEqual(Number(2), Number(3)), "(2 != 3)"),
      TestCase("And", And(Bool(true), Bool(false)), "(true && false)"),
      TestCase("Or", Or(Bool(true), Bool(false)), "(true || false)"),
      TestCase("FunctionCallExpr", FunctionCallExpr("f", List(Identifier("a", INT), Identifier("b", INT), Identifier("c", INT)), BrboType.INT), "f(a, b, c)"),
      TestCase("ITEExpr", ITEExpr(Bool(true), Number(0), Number(1)), "true ? 0 : 1")
    )
}

package brbo.common.ast

import brbo.TestCase
import brbo.common.StringCompare
import brbo.common.BrboType
import brbo.common.BrboType.INT
import org.scalatest.flatspec.AnyFlatSpec

class BrboExprUnitTest extends AnyFlatSpec {
  BrboExprUnitTest.prettyPrintToCUnitTest.foreach({
    testCase =>
      StringCompare.compareLiteral(testCase.input.asInstanceOf[BrboExpr].prettyPrintToC(), testCase.expectedOutput, s"${testCase.name} failed!")
  })
}

object BrboExprUnitTest {
  val prettyPrintToCUnitTest: List[TestCase] =
    List[TestCase](
      TestCase("Identifier", Identifier("z", BrboType.INT), "z"),
      TestCase("Boolean", Bool(true), "true"),
      TestCase("Number", Number(23), "23"),
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
      TestCase("FunctionCallExpr", FunctionCallExpr("f", List(Identifier("a", INT), Identifier("b", INT), Identifier("c", INT)), BrboType.INT), "f(a, b, c)")
    )
}

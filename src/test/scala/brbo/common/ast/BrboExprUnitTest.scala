package brbo.common.ast

import brbo.StringCompare
import brbo.common.TypeUtils.BrboType
import org.scalatest.flatspec.AnyFlatSpec

class BrboExprUnitTest extends AnyFlatSpec {
  BrboExprUnitTest.prettyPrintToCUnitTest.foreach({
    testCase =>
      StringCompare.compareLiteral(testCase.input.prettyPrintToC(), testCase.expectedOutput, s"${testCase.name} failed!")
  })
}

object BrboExprUnitTest {
  val prettyPrintToCUnitTest: List[BrboExprTestCase] =
    List[BrboExprTestCase](
      BrboExprTestCase("Identifier", Identifier("z", BrboType.INT), "z"),
      BrboExprTestCase("Boolean", Bool(true), "true"),
      BrboExprTestCase("Number", Number(23), "23"),
      BrboExprTestCase("Addition", Addition(Number(2), Number(3)), "(2 + 3)"),
      BrboExprTestCase("Subtraction", Subtraction(Number(2), Number(3)), "(2 - 3)"),
      BrboExprTestCase("Multiplication", Multiplication(Number(2), Number(3)), "(2 * 3)"),
      BrboExprTestCase("Division", Division(Number(2), Number(3)), "(2 / 3)"),
      BrboExprTestCase("Negative", Negative(Bool(true)), "!(true)"),
      BrboExprTestCase("LessThan", LessThan(Number(2), Number(3)), "(2 < 3)"),
      BrboExprTestCase("LessThanOrEqualTo", LessThanOrEqualTo(Number(2), Number(3)), "(2 <= 3)"),
      BrboExprTestCase("GreaterThan", GreaterThan(Number(2), Number(3)), "(2 > 3)"),
      BrboExprTestCase("GreaterThanOrEqualTo", GreaterThanOrEqualTo(Number(2), Number(3)), "(2 >= 3)"),
      BrboExprTestCase("Equal", Equal(Number(2), Number(3)), "(2 == 3)"),
      BrboExprTestCase("NotEqual", NotEqual(Number(2), Number(3)), "(2 != 3)"),
      BrboExprTestCase("And", And(Bool(true), Bool(false)), "(true && false)"),
      BrboExprTestCase("Or", Or(Bool(true), Bool(false)), "(true || false)"),
      BrboExprTestCase("FunctionCallExpr", FunctionCallExpr("f", List("a", "b", "c"), BrboType.INT), "f(a, b, c)")
    )
}

case class BrboExprTestCase(name: String, input: BrboExpr, expectedOutput: String)
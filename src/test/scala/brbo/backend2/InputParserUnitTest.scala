package brbo.backend2

import brbo.TestCase
import brbo.backend2.InputParserUnitTest.{parseTests, toJsonTests}
import brbo.common.BrboType
import brbo.common.ast._
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class InputParserUnitTest extends AnyFlatSpec {
  "Parsing JSON format inputs" should "be correct" in {
    parseTests.foreach({
      testCase =>
        val result = InputParser.parse(testCase.input.asInstanceOf[String])
        val resultString = result.map(list => list.map(v => v.printToIR()))
        StringCompare.ignoreWhitespaces(resultString, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Converting a Brbo value into Json" should "be correct" in {
    toJsonTests.foreach({
      testCase =>
        val result = InputParser.toJson(testCase.input.asInstanceOf[BrboValue])
        StringCompare.ignoreWhitespaces(result.toString(), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object InputParserUnitTest {
  private val parseTests: List[TestCase] = List(
    TestCase("Test 01",
      """[
        |  [
        |    1,
        |    [1,2,3],
        |    false,
        |    true,
        |    2,
        |    [3,4,5]
        |  ],
        |  [
        |    3,
        |    4,
        |    true,
        |    [1,2,3]
        |  ]
        |]""".stripMargin,
      """List(1, [1,2,3], false, true, 2, [3,4,5])
        |List(3, 4, true, [1,2,3])""".stripMargin),
    TestCase("Test 02", "[]", """"""),
    TestCase("Test 02", "[[], [1, false]]",
      """List()
        |List(1, false)""".stripMargin)
  )

  private val toJsonTests = List(
    TestCase("Number", Number(5), """5"""),
    TestCase("Bool", Bool(b = true), """true"""),
    TestCase("Array", BrboArray(values = List(Number(5), Number(1)), BrboType.INT), """[5,1]"""),
  )
}

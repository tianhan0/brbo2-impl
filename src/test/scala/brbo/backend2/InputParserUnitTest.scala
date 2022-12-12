package brbo.backend2

import brbo.TestCase
import brbo.backend2.InputParserUnitTest.tests
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class InputParserUnitTest extends AnyFlatSpec {
  "Parsing JSON format inputs" should "be correct" in {
    tests.foreach({
      test =>
        val result = InputParser.parse(test.input.asInstanceOf[String])
        val resultString = result.map(list => list.map(v => v.printToIR()))
        StringCompare.ignoreWhitespaces(resultString, test.expectedOutput, s"${test.name} failed")
    })
  }
}

object InputParserUnitTest {
  val tests: List[TestCase] = List(
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
      """List(1, {1,2,3}, false, true, 2, {3,4,5})
        |List(3, 4, true, {1,2,3})""".stripMargin),
    TestCase("Test 02", "[]", """"""),
    TestCase("Test 02", "[[], [1, false]]",
      """List()
        |List(1, false)""".stripMargin)
  )
}

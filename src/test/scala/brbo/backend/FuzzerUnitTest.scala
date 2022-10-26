package brbo.backend

import brbo.common.BrboType
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class FuzzerUnitTest extends AnyFlatSpec {
  "Enumerating values of a given type" should "be correct" in {
    val maxArrayLength = 5
    val maxInteger = 1000
    val possibilities = 10
    val booleans = Fuzzer.randomValues(BrboType.BOOL, maxArrayLength, maxInteger, possibilities)
    val booleansExpected =
      """false
        |true""".stripMargin
    StringCompare.ignoreWhitespaces(booleans, booleansExpected, "Numerating booleans failed")

    val integers = Fuzzer.randomValues(BrboType.INT, maxArrayLength, maxInteger, possibilities)
    StringCompare.ignoreWhitespaces(integers.length.toString, "11", "Numerating integers failed")

    val arrays = Fuzzer.randomValues(BrboType.ARRAY(BrboType.INT), maxArrayLength, maxInteger, possibilities = 2)
    StringCompare.ignoreWhitespaces(arrays.toString, "363", "Numerating integer arrays failed")
  }
}

object FuzzerUnitTest {

}

package brbo.backend2

import brbo.common.BrboType
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class FuzzerUnitTest extends AnyFlatSpec {
  "Enumerating values of a given type" should "be correct" in {
    val maxArrayLength = 5
    val samples = 10
    val fuzzer = new Fuzzer(maxInteger = 30, minInteger = 0)
    val seed = 62618
    val booleans = fuzzer.randomValues(BrboType.BOOL, samples, maxArrayLength = maxArrayLength, seed = seed).map(v => v.printToIR())
    val booleansExpected =
      """false
        |false
        |false
        |false
        |true
        |true
        |true
        |true
        |true
        |true""".stripMargin
    StringCompare.ignoreWhitespaces(booleans, booleansExpected, "Enumerating booleans failed")

    val integers = fuzzer.randomValues(BrboType.INT, samples, maxArrayLength = maxArrayLength, seed = seed).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces(integers,
      """1
        |11
        |14
        |17
        |17
        |20
        |26
        |28
        |29
        |3""".stripMargin, "Enumerating integers failed")

    /*val arrays = fuzzer.randomValues(BrboType.ARRAY(BrboType.INT), samples, maxArrayLength = maxArrayLength, seed = seed).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces(arrays,
      """{1,12,9,20,16,28}
        |{1,12,9,20,16}
        |{12,9,20,16,28,24}
        |{17,28}
        |{17}
        |{21}
        |{25,5,1,12}
        |{25,5,1}
        |{28,25,5}
        |{28,25}
        |{5,1,12,9,20}
        |{5,1,12,9}""".stripMargin, "Enumerating integer arrays failed")*/
  }
}

object FuzzerUnitTest {

}

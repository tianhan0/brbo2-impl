package brbo.backend2

import brbo.common.BrboType
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class FuzzerUnitTest extends AnyFlatSpec {
  "Enumerating values of a given type" should "be correct" in {
    val samples = 10
    val fuzzer = new Fuzzer(maxInteger = 30, minInteger = 0, maxArrayLength = 5)
    val seed = 62618
    val booleans = fuzzer.randomValues(BrboType.BOOL, samples, seed = seed).map(v => v.printToIR())
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

    val integers = fuzzer.randomValues(BrboType.INT, samples, seed = seed).map(v => v.printToIR())
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

    val arrays = fuzzer.randomValues(BrboType.ARRAY(BrboType.INT), samples, seed = seed).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces(arrays,
      """{1,29}
        |{12}
        |{13}
        |{16,25,6,26,17}
        |{17,0,6}
        |{17}
        |{24,27,15,29,11}
        |{25,0,13,7}
        |{28}
        |{5,0}""".stripMargin, "Enumerating integer arrays failed")
  }
}

object FuzzerUnitTest {

}

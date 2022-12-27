package brbo.backend2

import brbo.common.BrboType
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class FuzzerUnitTest extends AnyFlatSpec {
  "Enumerating values of a given type" should "be correct" in {
    val samples = 10
    val fuzzer = new Fuzzer(maxInteger = 30, minInteger = 0, maxArrayLength = 5, minArrayLength = 2)
    val booleans = fuzzer.randomValues(BrboType.BOOL, samples, seed = Fuzzer.SEED).map(v => v.printToIR())
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

    val integers = fuzzer.randomValues(BrboType.INT, samples, seed = Fuzzer.SEED).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces(integers,
      """1
        |12
        |16
        |16
        |19
        |24
        |27
        |28
        |4
        |8""".stripMargin, "Enumerating integers failed")

    val integersSeed1 = fuzzer.randomValues(BrboType.INT, samples, seed = 1).map(v => v.printToIR())
    val integersSeed2 = fuzzer.randomValues(BrboType.INT, samples, seed = 2).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces((integersSeed1 == integersSeed2).toString, "false",
      message = s"$integersSeed1 should differ from $integersSeed2")

    val arrays = fuzzer.randomValues(BrboType.ARRAY(BrboType.INT), samples, seed = Fuzzer.SEED).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces(arrays,
      """[0,12,8]
        |[10,6]
        |[12,24,19,0]
        |[2,14,10,21,18]
        |[26,22]
        |[30,10]
        |[30,25,6,2,14]
        |[4,28,9]
        |[8,20,15]
        |[9,5,17]""".stripMargin, "Enumerating integer arrays failed")
  }
}

object FuzzerUnitTest {

}

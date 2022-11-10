package brbo.backend2

import brbo.common.BrboType
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class FuzzerUnitTest extends AnyFlatSpec {
  "Enumerating values of a given type" should "be correct" in {
    val maxArrayLength = 5
    val maxInteger = 100
    val samples = 10
    val booleans = Fuzzer.randomValues(BrboType.BOOL, samples, maxArrayLength = maxArrayLength, maxInteger = maxInteger).map(v => v.printToIR())
    val booleansExpected =
      """false
        |true""".stripMargin
    StringCompare.ignoreWhitespaces(booleans, booleansExpected, "Numerating booleans failed")

    val integers = Fuzzer.randomValues(BrboType.INT, samples, maxArrayLength = maxArrayLength, maxInteger = maxInteger).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces(integers,
      """10
        |20
        |33
        |41
        |49
        |49
        |55
        |6
        |67
        |71
        |95""".stripMargin, "Numerating integers failed")

    val arrays = Fuzzer.randomValues(BrboType.ARRAY(BrboType.INT), samples, maxArrayLength = maxArrayLength, maxInteger = maxInteger).map(v => v.printToIR())
    StringCompare.ignoreWhitespaces(arrays,
      """{11,36,64,89}
        |{11,36,64}
        |{34}
        |{36,64,89,13,38}
        |{36,64,89,13}
        |{62,87}
        |{62}
        |{64,89,13,38,66,92}
        |{64,89,13,38,66}
        |{87,11,36}
        |{87,11}
        |{89,13,38,66,92,15}""".stripMargin, "Numerating integer arrays failed")
  }
}

object FuzzerUnitTest {

}

package brbo.backend2.qfuzz

import brbo.common.BrboType
import brbo.common.ast.Identifier
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class ExecutorUnitTest extends AnyFlatSpec {
  "Parsing an array of integers into input values" should "be correct" in {
    val parameters = List(
      Identifier("a", BrboType.INT),
      Identifier("b", BrboType.ARRAY(BrboType.INT)),
      Identifier("c", BrboType.INT),
      Identifier("d", BrboType.ARRAY(BrboType.INT)),
      Identifier("e", BrboType.BOOL),
      Identifier("f", BrboType.BOOL),
    )
    val inputArray = List(
      1,
      2, 3, 4, 5, 6, 7, 8, 9,
      10,
      11, 12, 13, 14, 15, 16, 17, 18,
      16000, 17000,
    )
    val result = Executor.toInputValues(parameters = parameters, inputArray = inputArray).get
    StringCompare.ignoreWhitespaces(result.map(v => v.printToIR()).mkString("\n"),
      """1
        |[2,3,4,5,6,7,8,9]
        |10
        |[11,12,13,14,15,16,17,18]
        |false
        |true""".stripMargin)
  }
}

package brbo.backend2.qfuzz

import brbo.backend2.qfuzz.DriverGenerator.GeneratorParameters
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
      2, 3, 4, 5, 6,
      10,
      11, 12, 13, 14, 15,
      16000, 17000,
    )
    val result = Executor.toInputValues(
      parameters = parameters,
      inputArray = inputArray,
      parametersInLoopConditions = List(Identifier("c", BrboType.INT)),
      generatorParameters = GeneratorParameters(arraySize = 5, minInteger = 4, maxInteger = 200, minLoopIterations = 2, maxLoopIterations = 3)
    ).get
    StringCompare.ignoreWhitespaces(result.map(v => v.printToIR()).mkString("\n"),
      """1
        |[2,3,4,5,6]
        |2
        |[11,12,13,14,15]
        |false
        |true""".stripMargin)
  }
}

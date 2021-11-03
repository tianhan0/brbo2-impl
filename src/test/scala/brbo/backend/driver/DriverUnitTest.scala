package brbo.backend.driver

import brbo.TestCase
import brbo.backend.driver.DriverUnitTest.testCases
import brbo.common.ast._
import brbo.common.{BrboType, CommandLineArguments}
import brbo.frontend.BasicProcessor
import org.scalatest.flatspec.AnyFlatSpec

class DriverUnitTest extends AnyFlatSpec {
  "Driver" should "correctly verify with selective amortization" in {
    testCases.foreach({
      testCase =>
        val (className, code, bound) = testCase.input.asInstanceOf[(String, String, BrboExpr)]
        val targetProgram = BasicProcessor.getTargetProgram(className, code)
        val driver = new Driver(CommandLineArguments.DEBUG_MODE_ARGUMENTS, targetProgram.program)
        driver.verifySelectivelyAmortize(bound)
    })
  }
}

object DriverUnitTest {
  val testCases: List[TestCase] = {
    val program1 =
      """class Test {
        |  void main(int n) {
        |    int R = 0;
        |    R = R + 1;
        |    R = R + 2;
        |    R = R + 3;
        |  }
        |}""".stripMargin
    val bound1 = Number(6)

    val program2 =
      """class Test {
        |  void main(int n) {
        |    int R = 0;
        |    int i = 0;
        |    while (i < n) {
        |      R = R + 1;
        |    }
        |    R = R + 2;
        |  }
        |}""".stripMargin
    val bound2 = Addition(Identifier("n", BrboType.INT), Number(2))

    List(
      TestCase("Driver Test 1", ("Test", program1, bound1), ""),
      // TestCase("Driver Test 2", ("Test", program2, bound2), ""),
    )
  }
}
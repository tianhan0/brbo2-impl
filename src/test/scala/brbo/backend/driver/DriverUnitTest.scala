package brbo.backend.driver

import brbo.TestCase
import brbo.backend.driver.DriverUnitTest.testCases
import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.common.CommandLineArguments._
import brbo.common.ast.BrboExprUtils.lessThanOrEqualTo
import brbo.common.ast._
import brbo.common.string.StringCompare
import brbo.common.{BrboType, CommandLineArguments}
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.scalatest.flatspec.AnyFlatSpec

/*class DriverUnitTest extends AnyFlatSpec {
  val arguments = new CommandLineArguments
  arguments.initialize(
    TEST_MODE,
    debugMode = DEFAULT_DEBUG_MODE,
    "",
    printVerifierInputs = DEFAULT_PRINT_VERIFIER_INPUTS,
    verifierTimeout = 20,
    printCFG = false,
    maxGroups = DEFAULT_MAX_GROUPS,
    verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
    relationalPredicates = DEFAULT_RELATIONAL_PREDICATES,
    maxIterations = 5,
    assertionTag = DEFAULT_ASSERTION_TAG,
    abstractDomain = DEFAULT_ABSTRACT_DOMAIN,
    maxPathLength = DEFAULT_MAX_PATH_LENGTH,
    checkWithZ3 = DEFAULT_CHECK_WITH_Z3,
    assumePositiveInputs = DEFAULT_ASSUME_POSITIVE_INPUTS,
    widenThreshold = DEFAULT_WIDEN_THRESHOLD,
    numberOfThreads = DEFAULT_NUMBER_OF_THREADS,
  )

  "Driver" should "correctly verify with selective amortization" in {
    testCases.foreach({
      testCase =>
        val (className, code, upperBound) = testCase.input.asInstanceOf[(String, String, BrboExpr)]
        val targetProgram = BasicProcessor.getTargetProgram(className, code)
        val driver = new Driver(arguments, targetProgram.program)
        val boundAssertion = BoundAssertion("R", lessThanOrEqualTo(Identifier("R", BrboType.INT), upperBound), tag = "IrrelevantTag")
        StringCompare.ignoreWhitespaces(driver.verifySelectivelyAmortize(boundAssertion).toString, """TRUE_RESULT""")
    })
  }
}*/

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
      s"""class Test {
        |  void main(int n) {
        |    if (n <= 0) // Otherwise, int overflow will cause unexpected cex.
        |      return;
        |    int R = 0;
        |    int i = 0;
        |    while (i < n) {
        |      R = R + 1;
        |      i = i + 1;
        |    }
        |    R = R + 2;
        |  }
        |}""".stripMargin
    val bound2 = Addition(Identifier("n", BrboType.INT), Number(2))

    List(
      // TestCase("Driver Test 1", ("Test", program1, bound1), ""),
      TestCase("Driver Test 2", ("Test", program2, bound2), ""),
    )
  }
}
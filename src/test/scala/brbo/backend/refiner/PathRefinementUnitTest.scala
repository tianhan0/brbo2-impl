package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.verifier.AmortizationMode.UNKNOWN_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.cex.Path
import brbo.common.CommandLineArguments.DEFAULT_MAX_ITERATIONS
import brbo.common.{CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class PathRefinementUnitTest extends AnyFlatSpec {
  private val arguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(
      UNKNOWN_MODE,
      debugMode = false,
      "",
      skipSanityCheck = false,
      printModelCheckerInputs = false,
      modelCheckerTimeout = 20,
      printCFG = false,
      lessPreciseBound = false,
      generateSynthetic = 0,
      maxGroups = 5,
      modelCheckerDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false,
      maxIterations = DEFAULT_MAX_ITERATIONS,
    )
    arguments
  }

  "Path refinements by inserting uses" should "be correct" in {
    PathRefinementUnitTest.testUses.foreach({
      testCase =>
        val pathRefinement = new PathRefinement(arguments)
        val paths = pathRefinement.replaceUseOnly(testCase.input.asInstanceOf[Path], PathRefinementTestCases.brboProgram.mainFunction.identifier)
        (StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
    })
  }

  "Path refinements by inserting resets" should "be correct" in {
    PathRefinementUnitTest.testResets.foreach({
      testCase =>
        val pathRefinement = new PathRefinement(arguments)
        val paths = pathRefinement.removeResetOnly(testCase.input.asInstanceOf[Refinement], PathRefinementTestCases.brboProgram.mainFunction.identifier)
        (StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
    })
  }
}

object PathRefinementUnitTest {
  val testUses: List[TestCase] = {
    List(
      TestCase("Test 01", PathRefinementTestCases.test01,
        """Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |  3: if (true) reset R2
          |  4: if (true) reset R3
          |  5: if (true) use R3 1
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 02", PathRefinementTestCases.test02,
        """Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |  3: if (true) reset R2
          |  4: if (true) use R2 1
          |  5: if (true) reset R3
          |  6: if (true) use R3 1
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 03", PathRefinementTestCases.test03,
        """Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |  3: if (true) reset R2
          |  4: if (true) use R2 1
          |  5: if (true) reset R2
          |  6: if (true) use R2 1
          |  7: if (true) reset R3
          |  8: if (true) use R3 1
          |Removed resets:
          |
          |Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |  3: if (true) reset R2
          |  4: if (true) use R2 1
          |  5: if (true) reset R3
          |  6: if (true) use R3 1
          |  7: if (true) reset R2
          |  8: if (true) use R2 1
          |Removed resets:
          |
          |Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |  3: if (true) reset R2
          |  4: if (true) use R2 1
          |  5: if (true) reset R3
          |  6: if (true) use R3 1
          |  7: if (true) reset R3
          |  8: if (true) use R3 1
          |Removed resets:
          |
          |Path:
          |  int R1 = 0; [Function `Main`]
          |  int S1 = 0; [Function `Main`]
          |  int C1 = -1; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |Splits:
          |  3: if (true) reset R2
          |  4: if (true) use R2 1
          |  5: if (true) reset R3
          |  6: if (true) use R3 1
          |  7: if (true) reset R4
          |  8: if (true) use R4 1
          |Removed resets:
          |""".stripMargin),
    )
  }

  val testResets: List[TestCase] = {
    List(
      TestCase("Test 07", PathRefinementTestCases.test07,
        """Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  0""".stripMargin),
      TestCase("Test 08", PathRefinementTestCases.test08,
        """Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  0
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  0
          |  2
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  2""".stripMargin),
      TestCase("Test 09", PathRefinementTestCases.test09,
        """Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  0
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  0
          |  2
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  0
          |  2
          |  4
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  0
          |  4
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  2
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  2
          |  4
          |Path:
          |  if (true) reset R1 [Function `Main`]
          |  if (true) use R1 1 [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  4""".stripMargin),
    )
  }
}
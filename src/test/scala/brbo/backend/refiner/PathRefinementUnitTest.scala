package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.verifier.AmortizationMode.UNKNOWN_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.cex.Path
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
      modelCheckerDirectory = UAutomizerVerifier.TOOL_DIRECTORY
    )
    arguments
  }

  "Path refinements by inserting uses" should "be correct" in {
    PathRefinementUnitTest.testUses.foreach({
      testCase =>
        val pathRefinement = new PathRefinement(arguments, PathRefinementTestCases.brboProgram)
        val paths = pathRefinement.insertUseOnly(testCase.input.asInstanceOf[Path])
        (StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
    })
  }

  "Path refinements by inserting resets" should "be correct" in {
    PathRefinementUnitTest.testResets.foreach({
      testCase =>
        val pathRefinement = new PathRefinement(arguments, PathRefinementTestCases.brboProgram)
        val paths = pathRefinement.removeResetOnly(testCase.input.asInstanceOf[Refinement])
        (StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
    })
  }
}

object PathRefinementUnitTest {
  val testUses: List[TestCase] = {
    List(
      TestCase("Test 01", PathRefinementTestCases.test01,
        """Path:
          |  reset R1 [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  reset R1 [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |  0: reset R2
          |  1: reset R3
          |  2: use R3 (R + 0)
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 02", PathRefinementTestCases.test02,
        """Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |  0: reset R2
          |  1: use R2 (R + 0)
          |  2: reset R3
          |  3: use R3 (R + 0)
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 03", PathRefinementTestCases.test03,
        """Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |  0: reset R2
          |  1: use R2 (R + 0)
          |  2: reset R2
          |  3: use R2 (R + 0)
          |  4: reset R3
          |  5: use R3 (R + 0)
          |Removed resets:
          |
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |  0: reset R2
          |  1: use R2 (R + 0)
          |  2: reset R3
          |  3: use R3 (R + 0)
          |  4: reset R2
          |  5: use R2 (R + 0)
          |Removed resets:
          |
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |  0: reset R2
          |  1: use R2 (R + 0)
          |  2: reset R3
          |  3: use R3 (R + 0)
          |  4: reset R3
          |  5: use R3 (R + 0)
          |Removed resets:
          |
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Splits:
          |  0: reset R2
          |  1: use R2 (R + 0)
          |  2: reset R3
          |  3: use R3 (R + 0)
          |  4: reset R4
          |  5: use R4 (R + 0)
          |Removed resets:
          |""".stripMargin),
    )
  }

  val testResets: List[TestCase] = {
    List(
      TestCase("Test 07", PathRefinementTestCases.test07,
        """Path:
          |
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  0""".stripMargin),
      TestCase("Test 08", PathRefinementTestCases.test08,
        """Path:
          |
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  0
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  0
          |  2
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  2""".stripMargin),
      TestCase("Test 09", PathRefinementTestCases.test09,
        """Path:
          |
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  0
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  0
          |  2
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  0
          |  2
          |  4
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  0
          |  4
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  2
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  2
          |  4
          |Path:
          |
          |Splits:
          |
          |Removed resets:
          |  4""".stripMargin),
    )
  }
}
package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.cex.Path
import brbo.common.CommandLineArguments.{DEFAULT_ASSERTION_INDEX, DEFAULT_MAX_ITERATIONS}
import brbo.common.{CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class PathRefinementUnitTest extends AnyFlatSpec {
  private val arguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(
      TEST_MODE,
      debugMode = false,
      "",
      skipSanityCheck = false,
      printVerifierInputs = false,
      verifierTimeout = 20,
      printCFG = false,
      generateSynthetic = 0,
      maxGroups = 5,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false,
      maxIterations = DEFAULT_MAX_ITERATIONS,
      assertionIndex = DEFAULT_ASSERTION_INDEX,
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
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R3
          |  [005] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R3 1
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 02", PathRefinementTestCases.test02,
        """Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R3 1
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 03", PathRefinementTestCases.test03,
        """Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [006] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R2 1
          |  [007] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R3
          |  [008] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R3 1
          |Removed resets:
          |
          |Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R3 1
          |  [007] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [008] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R2 1
          |Removed resets:
          |
          |Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R3 1
          |  [007] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R3
          |  [008] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R3 1
          |Removed resets:
          |
          |Path:
          |  (-1) int R1 = 0; [Function `Main`]
          |  (-1) int S1 = 0; [Function `Main`]
          |  (-1) int C1 = -1; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R3 1
          |  [007] (-1) if (true) reset R1 [Function `Main`] -> if (true) reset R4
          |  [008] (-1) if (true) use R1 1 [Function `Main`] -> if (true) use R4 1
          |Removed resets:
          |""".stripMargin),
    )
  }

  val testResets: List[TestCase] = {
    List(
      TestCase("Test 07", PathRefinementTestCases.test07,
        """Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [Function `Main`]""".stripMargin),
      TestCase("Test 08", PathRefinementTestCases.test08,
        """Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [Function `Main`]
          |  [002]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [002]: (-1) if (true) reset R1 [Function `Main`]""".stripMargin),
      TestCase("Test 09", PathRefinementTestCases.test09,
        """Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [Function `Main`]
          |  [002]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [Function `Main`]
          |  [002]: (-1) if (true) reset R1 [Function `Main`]
          |  [004]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [Function `Main`]
          |  [004]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [002]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [002]: (-1) if (true) reset R1 [Function `Main`]
          |  [004]: (-1) if (true) reset R1 [Function `Main`]
          |Path:
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) if (true) use R1 1 [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |  (-1) x = 0; [Function `Main`]
          |  (-1) if (true) reset R1 [Function `Main`]
          |Splits:
          |
          |Removed resets:
          |  [004]: (-1) if (true) reset R1 [Function `Main`]""".stripMargin),
    )
  }
}
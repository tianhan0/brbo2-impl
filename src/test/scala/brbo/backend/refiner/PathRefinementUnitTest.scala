package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.common.commandline.Arguments
import brbo.common.commandline.Arguments._
import org.scalatest.flatspec.AnyFlatSpec

class PathRefinementUnitTest extends AnyFlatSpec {
  private val arguments = {
    val arguments = new Arguments
    arguments.initialize(
      TEST_MODE,
      debugMode = DEFAULT_DEBUG_MODE,
      "",
      printVerifierInputs = DEFAULT_PRINT_VERIFIER_INPUTS,
      verifierTimeout = 20,
      printCFG = false,
      maxGroups = 5,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = DEFAULT_RELATIONAL_PREDICATES,
      maxIterations = DEFAULT_MAX_ITERATIONS,
      assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = DEFAULT_ABSTRACT_DOMAIN,
      maxPathLength = DEFAULT_MAX_PATH_LENGTH,
      checkWithZ3 = DEFAULT_CHECK_WITH_Z3,
      assumePositiveInputs = DEFAULT_ASSUME_POSITIVE_INPUTS,
      widenThreshold = DEFAULT_WIDEN_THRESHOLD,
      numberOfThreads = DEFAULT_NUMBER_OF_THREADS,
    )
    arguments
  }

  /*"Path refinements by inserting uses" should "be correct" in {
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
  }*/
}

object PathRefinementUnitTest {
  val testUses: List[TestCase] = {
    List(
      TestCase("Test 01", PathRefinementTestCases.test01,
        """Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |  [005] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |  [005] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R3
          |  [005] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R3 1
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 02", PathRefinementTestCases.test02,
        """Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) use R1 1 [fun `Main`]
          |  [005] (-1) if (true) reset R1 [fun `Main`]
          |  [006] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) use R1 1 [fun `Main`]
          |  [005] (-1) if (true) reset R1 [fun `Main`]
          |  [006] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R3 1
          |Removed resets:
          |""".stripMargin),
      TestCase("Test 03", PathRefinementTestCases.test03,
        """Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) use R1 1 [fun `Main`]
          |  [005] (-1) if (true) reset R1 [fun `Main`]
          |  [006] (-1) if (true) use R1 1 [fun `Main`]
          |  [007] (-1) if (true) reset R1 [fun `Main`]
          |  [008] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) use R1 1 [fun `Main`]
          |  [005] (-1) if (true) reset R1 [fun `Main`]
          |  [006] (-1) if (true) use R1 1 [fun `Main`]
          |  [007] (-1) if (true) reset R1 [fun `Main`]
          |  [008] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [006] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R2 1
          |  [007] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R3
          |  [008] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R3 1
          |Removed resets:
          |
          |Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) use R1 1 [fun `Main`]
          |  [005] (-1) if (true) reset R1 [fun `Main`]
          |  [006] (-1) if (true) use R1 1 [fun `Main`]
          |  [007] (-1) if (true) reset R1 [fun `Main`]
          |  [008] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R3 1
          |  [007] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [008] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R2 1
          |Removed resets:
          |
          |Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) use R1 1 [fun `Main`]
          |  [005] (-1) if (true) reset R1 [fun `Main`]
          |  [006] (-1) if (true) use R1 1 [fun `Main`]
          |  [007] (-1) if (true) reset R1 [fun `Main`]
          |  [008] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R3 1
          |  [007] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R3
          |  [008] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R3 1
          |Removed resets:
          |
          |Path:
          |  [000] (-1) int R1 = 0; [fun `Main`]
          |  [001] (-1) int S1 = 0; [fun `Main`]
          |  [002] (-1) int C1 = -1; [fun `Main`]
          |  [003] (-1) if (true) reset R1 [fun `Main`]
          |  [004] (-1) if (true) use R1 1 [fun `Main`]
          |  [005] (-1) if (true) reset R1 [fun `Main`]
          |  [006] (-1) if (true) use R1 1 [fun `Main`]
          |  [007] (-1) if (true) reset R1 [fun `Main`]
          |  [008] (-1) if (true) use R1 1 [fun `Main`]
          |Splits:
          |  [003] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R2
          |  [004] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R2 1
          |  [005] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R3
          |  [006] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R3 1
          |  [007] (-1) if (true) reset R1 [fun `Main`] -> if (true) reset R4
          |  [008] (-1) if (true) use R1 1 [fun `Main`] -> if (true) use R4 1
          |Removed resets:
          |""".stripMargin),
    )
  }

  val testResets: List[TestCase] = {
    List(
      TestCase("Test 07", PathRefinementTestCases.test07,
        """Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) x = 0; [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) x = 0; [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [fun `Main`]""".stripMargin),
      TestCase("Test 08", PathRefinementTestCases.test08,
        """Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [fun `Main`]
          |  [002]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [002]: (-1) if (true) reset R1 [fun `Main`]""".stripMargin),
      TestCase("Test 09", PathRefinementTestCases.test09,
        """Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [fun `Main`]
          |  [002]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [fun `Main`]
          |  [002]: (-1) if (true) reset R1 [fun `Main`]
          |  [004]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [000]: (-1) if (true) reset R1 [fun `Main`]
          |  [004]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [002]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [002]: (-1) if (true) reset R1 [fun `Main`]
          |  [004]: (-1) if (true) reset R1 [fun `Main`]
          |Path:
          |  [000] (-1) if (true) reset R1 [fun `Main`]
          |  [001] (-1) if (true) use R1 1 [fun `Main`]
          |  [002] (-1) if (true) reset R1 [fun `Main`]
          |  [003] (-1) x = 0; [fun `Main`]
          |  [004] (-1) if (true) reset R1 [fun `Main`]
          |Splits:
          |
          |Removed resets:
          |  [004]: (-1) if (true) reset R1 [fun `Main`]""".stripMargin),
    )
  }
}
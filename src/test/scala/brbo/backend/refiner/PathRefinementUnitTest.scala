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

  "Inserting uses" should "be correct" in {
    PathRefinementUnitTest.testUses.foreach({
      testCase =>
        val pathRefinement = new PathRefinement(arguments, PathRefinementTestCases.brboProgram)
        val paths = pathRefinement.insertUseOnly(testCase.input.asInstanceOf[Path])
        assert(StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
    })
  }

  "Inserting resets" should "be correct" in {
    PathRefinementUnitTest.testResets.foreach({
      testCase =>
        val pathRefinement = new PathRefinement(arguments, PathRefinementTestCases.brboProgram)
        val paths = pathRefinement.removeResetOnly(testCase.input.asInstanceOf[Path])
        assert(StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
    })
  }
}

object PathRefinementUnitTest {
  val testUses: List[TestCase] = {
    List(
      TestCase("Test 01", PathRefinementTestCases.test01,
        """Path:
          |  x = 0; [Function `Main`]
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  reset R4 [Function `Main`]
          |  use R4 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  reset R4 [Function `Main`]
          |  use R4 (R + 0) [Function `Main`]
          |  reset R5 [Function `Main`]
          |  use R5 (R + 0) [Function `Main`]""".stripMargin),
      TestCase("Test 02", PathRefinementTestCases.test02,
        """Path:
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  reset R4 [Function `Main`]
          |  use R4 (R + 0) [Function `Main`]
          |  reset R5 [Function `Main`]
          |  use R5 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  reset R4 [Function `Main`]
          |  use R4 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R2 [Function `Main`]
          |  use R2 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]""".stripMargin),
      TestCase("Test 03", PathRefinementTestCases.test03,
        """Path:
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  reset R4 [Function `Main`]
          |  use R4 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R5 [Function `Main`]
          |  use R5 (R + 0) [Function `Main`]
          |Path:
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R4 [Function `Main`]
          |  use R4 (R + 0) [Function `Main`]
          |Path:
          |  reset R3 [Function `Main`]
          |  use R3 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R4 [Function `Main`]
          |  use R4 (R + 0) [Function `Main`]
          |  reset R5 [Function `Main`]
          |  use R5 (R + 0) [Function `Main`]""".stripMargin),
    )
  }

  val testResets: List[TestCase] = {
    List(
      TestCase("Test 07", PathRefinementTestCases.test07,
        """Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]""".stripMargin),
      TestCase("Test 08", PathRefinementTestCases.test08,
        """Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]""".stripMargin),
      TestCase("Test 09", PathRefinementTestCases.test09,
        """Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]
          |Path:
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R1 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]
          |Path:
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]""".stripMargin),
    )
  }
}
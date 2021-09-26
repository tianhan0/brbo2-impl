package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.verifier.cex.Path
import brbo.common.{CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class PathRefinementOldUnitTest extends AnyFlatSpec {
  PathRefinementOldUnitTest.testCases.foreach({
    testCase =>
      val pathRefinementOld = new PathRefinementOld(CommandLineArguments.DEFAULT_ARGUMENTS, PathRefinementTestCases.brboProgram)
      val paths = pathRefinementOld.refine(testCase.input.asInstanceOf[Path])
      assert(StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
  })
}

object PathRefinementOldUnitTest {
  val testCases: List[TestCase] = {
    List(
      TestCase("Test 01", PathRefinementTestCases.test01,
        """Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]""".stripMargin),
      TestCase("Test 02", PathRefinementTestCases.test02,
        """Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]""".stripMargin),
      TestCase("Test 03", PathRefinementTestCases.test03,
        """Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]""".stripMargin),
      TestCase("Test 04", PathRefinementTestCases.test04,
        """Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]""".stripMargin),
      TestCase("Test 05", PathRefinementTestCases.test05,
        """Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  use R0 (R + 0) [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]""".stripMargin),
      TestCase("Test 06", PathRefinementTestCases.test06,
        """Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R0 [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  reset R1 [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |Path:
          |  x = 0; [Function `Main`]
          |  use R0 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]
          |  use R1 (R + 0) [Function `Main`]
          |  x = 0; [Function `Main`]""".stripMargin),
    )
  }
}

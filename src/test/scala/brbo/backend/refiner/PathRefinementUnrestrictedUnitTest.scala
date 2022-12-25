package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.verifier.cex.Path
import brbo.common.commandline.Arguments
import org.scalatest.flatspec.AnyFlatSpec

class PathRefinementUnrestrictedUnitTest extends AnyFlatSpec {
  /*PathRefinementOldUnitTest.testCases.foreach({
    testCase =>
      val pathRefinementOld = new PathRefinementOld(CommandLineArguments.DEFAULT_ARGUMENTS, PathRefinementTestCases.brboProgram)
      val paths = pathRefinementOld.refine(testCase.input.asInstanceOf[Path])
      assert(StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
  })*/
}

object PathRefinementUnrestrictedUnitTest {
  val testCases: List[TestCase] = {
    List(
      TestCase("Test 01", PathRefinementTestCases.test01,
        """Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]""".stripMargin),
      TestCase("Test 02", PathRefinementTestCases.test02,
        """Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]""".stripMargin),
      TestCase("Test 03", PathRefinementTestCases.test03,
        """Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]""".stripMargin),
      TestCase("Test 04", PathRefinementTestCases.test04,
        """Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]""".stripMargin),
      TestCase("Test 05", PathRefinementTestCases.test05,
        """Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  use R0 (R + 0) [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]""".stripMargin),
      TestCase("Test 06", PathRefinementTestCases.test06,
        """Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R0 [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  reset R1 [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |Path:
          |  x = 0; [fun `Main`]
          |  use R0 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]
          |  use R1 (R + 0) [fun `Main`]
          |  x = 0; [fun `Main`]""".stripMargin),
    )
  }
}

package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.verifier.cex.Path
import brbo.common.GhostVariableTyp.Resource
import brbo.common.{CommandLineArguments, GhostVariableUtils, StringCompare}
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.scalatest.flatspec.AnyFlatSpec

class PathRefinementUnitTest extends AnyFlatSpec {
  PathRefinementUnitTest.testCases.foreach({
    testCase =>
      val pathTransformation = new PathRefinement(CommandLineArguments.DEFAULT_ARGUMENTS, PathRefinementUnitTest.brboProgram)
      val paths = pathTransformation.refine(testCase.input.asInstanceOf[Path])
      assert(StringCompare.ignoreWhitespaces(paths, testCase.expectedOutput, s"`${testCase.name}` failed"))
  })
}

object PathRefinementUnitTest {
  val brboProgram: BrboProgram = BrboProgram("Test program", BrboFunction("Main", VOID, Nil, Block(List(Skip()))))

  val testCases: List[TestCase] = {
    val R = Identifier(GhostVariableUtils.generateName("", Resource), INT)
    val update = Addition(R, Number(0))
    val assignment = Assignment(R, update)
    val use1 = CFGNode(Left(Use(Some(1), update, assignment)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
    val use2 = CFGNode(Left(Use(Some(2), update, assignment)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
    // val use3 = CFGNode(Left(Use(Some(3), update, assignment)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
    val assignment2 = CFGNode(Left(Assignment(Identifier("x", INT), Number(0))), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)

    val test01 = Path(List(assignment2, use1))
    val test02 = Path(List(use1, assignment2))
    val test03 = Path(List(use1, assignment2, use2))
    val test04 = Path(List(assignment2, use1, use2))
    val test05 = Path(List(use1, use2, assignment2))
    val test06 = Path(List(assignment2, use1, assignment2, use2, assignment2))

    List(
      TestCase("Test 01", test01, """Path:
                                    |  x = 0; [Function `Main`]
                                    |  reset R0 [Function `Main`]
                                    |  use R0 (R + 0) [Function `Main`]
                                    |Path:
                                    |  x = 0; [Function `Main`]
                                    |  use R0 (R + 0) [Function `Main`]""".stripMargin),
      TestCase("Test 02", test02, """Path:
                                    |  reset R0 [Function `Main`]
                                    |  use R0 (R + 0) [Function `Main`]
                                    |  x = 0; [Function `Main`]
                                    |Path:
                                    |  use R0 (R + 0) [Function `Main`]
                                    |  x = 0; [Function `Main`]""".stripMargin),
      TestCase("Test 03", test03, """Path:
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
      TestCase("Test 04", test04, """Path:
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
      TestCase("Test 05", test05, """Path:
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
      TestCase("Test 06", test06, """Path:
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

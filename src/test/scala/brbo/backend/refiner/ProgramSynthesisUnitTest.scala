package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.refiner.ProgramSynthesisUnitTest.testCases
import brbo.common.BrboType.{INT, VOID}
import brbo.common.CommandLineArguments
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.scalatest.flatspec.AnyFlatSpec

class ProgramSynthesisUnitTest extends AnyFlatSpec {
  testCases.foreach({
    testCase =>
      val programSynthesis = new ProgramSynthesis(ProgramSynthesisUnitTest.program, CommandLineArguments.DEBUG_MODE_ARGUMENTS)
      programSynthesis.synthesize(testCase.input.asInstanceOf[Refinement])
  })
}

object ProgramSynthesisUnitTest {
  private val i: Identifier = Identifier("i", INT)
  private val n: Identifier = Identifier("n", INT)

  private val reset: Reset = Reset(1)
  private val use: Use = Use(Some(1), Number(1), GreaterThanOrEqualTo(n, Number(0)))
  private val declaration = VariableDeclaration(i, Number(0))
  private val increment = Assignment(i, Addition(i, Number(1)))
  private val condition = LessThan(i, n)
  private val loop = Loop(condition, Block(List(reset, use, increment)))

  val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n), Block(List(declaration, loop)), Set(1, 2, 3))
  val program: BrboProgram = BrboProgram("Test program", mainFunction)

  val testCases: List[TestCase] = {
    val path = List(
      CFGNode(Left(VariableDeclaration(use.resourceVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(VariableDeclaration(reset.sharpVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(VariableDeclaration(reset.counterVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(declaration), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Right(condition), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
    )

    val splitUsesTest01 = {
      val reset2 = CFGNode(Left(Reset(2)), mainFunction, CFGNode.DONT_CARE_ID)
      val use2 = CFGNode(Left(Use(Some(2), use.update, use.condition)), mainFunction, CFGNode.DONT_CARE_ID)
      val reset3 = CFGNode(Left(Reset(3)), mainFunction, CFGNode.DONT_CARE_ID)
      val use3 = CFGNode(Left(Use(Some(3), use.update, use.condition)), mainFunction, CFGNode.DONT_CARE_ID)
      Map(2 -> ResetNode(reset2, 2), 3 -> UseNode(use2, 2), 4 -> ResetNode(reset3, 3), 5 -> UseNode(use3, 3))
    }
    val groupIdsTest01 = Map(1 -> Set(2, 3))

    List(
      TestCase("Split Uses", Refinement(path, splitUsesTest01, Set(), groupIdsTest01), """"""),
      TestCase("Remove Resets", Refinement(path, Map(), Set(2), Map()), """"""),
      TestCase("Split Uses and Remove Resets", Refinement(path, splitUsesTest01, Set(2), groupIdsTest01), """"""),
    )
  }
}

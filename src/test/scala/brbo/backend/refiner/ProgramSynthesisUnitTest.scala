package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.refiner.ProgramSynthesisUnitTest.{coverTests, disjointTests, synthesizeTests}
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{BrboType, CommandLineArguments, StringCompare, Z3Solver}
import org.scalatest.flatspec.AnyFlatSpec

class ProgramSynthesisUnitTest extends AnyFlatSpec {
  "Synthesizing programs" should "succeed" in {
    val programSynthesis = new ProgramSynthesis(ProgramSynthesisUnitTest.program, CommandLineArguments.DEBUG_MODE_ARGUMENTS)
    synthesizeTests.foreach({
      testCase =>
        programSynthesis.synthesize(testCase.input.asInstanceOf[Refinement]).mainFunction
    })
  }

  "Deciding disjointness" should "be correct" in {
    disjointTests.foreach({
      testCase =>
        val predicates = testCase.input.asInstanceOf[Iterable[Predicate]]
        val solver = new Z3Solver
        StringCompare.ignoreWhitespaces(ProgramSynthesis.isDisjoint(predicates, solver).toString, testCase.expectedOutput, s"Test case `${testCase.name}` failed!")
    })
  }

  "Deciding covers" should "be correct" in {
    coverTests.foreach({
      testCase =>
        val predicates = testCase.input.asInstanceOf[Iterable[Predicate]]
        val solver = new Z3Solver
        ProgramSynthesis.isCover(predicates, solver)
    })
  }
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

  val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n), Block(List(declaration, loop)), Set(1))
  val program: BrboProgram = BrboProgram("Test program", mainFunction)

  val synthesizeTests: List[TestCase] = {
    val path = List(
      CFGNode(Left(VariableDeclaration(reset.resourceVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID), // 0
      CFGNode(Left(VariableDeclaration(reset.sharpVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID), // 1
      CFGNode(Left(VariableDeclaration(reset.counterVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID), // 2
      CFGNode(Left(declaration), mainFunction, CFGNode.DONT_CARE_ID), // 3
      CFGNode(Right(condition), mainFunction, CFGNode.DONT_CARE_ID), // 4
      CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID), // 5
      CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID), // 6
      CFGNode(Left(increment), mainFunction, CFGNode.DONT_CARE_ID), // 7
      CFGNode(Right(condition), mainFunction, CFGNode.DONT_CARE_ID), // 8
      CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID), // 9
      CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID), // 10
    )

    val splitUsesTest01 = {
      val reset2 = CFGNode(Left(Reset(2)), mainFunction, CFGNode.DONT_CARE_ID)
      val use2 = CFGNode(Left(Use(Some(2), use.update, use.condition)), mainFunction, CFGNode.DONT_CARE_ID)
      val reset3 = CFGNode(Left(Reset(3)), mainFunction, CFGNode.DONT_CARE_ID)
      val use3 = CFGNode(Left(Use(Some(3), use.update, use.condition)), mainFunction, CFGNode.DONT_CARE_ID)
      Map(5 -> ResetNode(reset2, 2), 6 -> UseNode(use2, 2), 9 -> ResetNode(reset3, 3), 10 -> UseNode(use3, 3))
    }
    val groupIdsTest01 = Map(1 -> Set(2, 3))

    List(
      TestCase("Split Uses", Refinement(path, splitUsesTest01, Set(), groupIdsTest01), """"""),
      TestCase("Remove Resets", Refinement(path, Map(), Set(5), Map()), """"""),
      TestCase("Split Uses and Remove Resets", Refinement(path, splitUsesTest01, Set(5), groupIdsTest01), """"""),
    )
  }

  val disjointTests: List[TestCase] = {
    val i = Identifier("i", BrboType.INT)
    val n = Identifier("n", BrboType.INT)
    val zero = Number(0)

    val list1 = List(Predicate(GreaterThan(zero, i)), Predicate(GreaterThan(i, zero)))
    val list2 = List(Predicate(GreaterThanOrEqualTo(zero, i)), Predicate(GreaterThan(i, zero)))
    val list3 = List(Predicate(GreaterThanOrEqualTo(i, zero)), Predicate(And(GreaterThan(n, zero), GreaterThan(zero, i))))
    val list4 = List(Predicate(And(GreaterThanOrEqualTo(n, zero), GreaterThan(zero, n))), Predicate(Bool(b = true)))
    List(
      TestCase("Disjoint Test 01", list1, """true"""),
      TestCase("Disjoint Test 02", list2, """true"""),
      TestCase("Disjoint Test 03", list3, """true"""),
      TestCase("Disjoint Test 04", list4, """true"""),
    )
  }

  val coverTests: List[TestCase] = {
    val i = Identifier("i", BrboType.INT)
    val n = Identifier("n", BrboType.INT)
    val zero = Number(0)

    val list1 = List(Predicate(GreaterThan(zero, n)), Predicate(GreaterThanOrEqualTo(n, zero)))
    val list2 = List(Predicate(GreaterThanOrEqualTo(zero, n)), Predicate(GreaterThanOrEqualTo(n, zero)))
    val list3 = List(Predicate(GreaterThanOrEqualTo(i, n)), Predicate(GreaterThanOrEqualTo(n, zero)))
    List(
      TestCase("Cover Test 01", list1, """true"""),
      TestCase("Cover Test 02", list2, """true"""),
      TestCase("Cover Test 03", list3, """false"""),
    )
  }
}

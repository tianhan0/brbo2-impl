package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.refiner.Refinement.{ResetNode, UseNode}
import brbo.backend.refiner.SynthesizerUnitTest.{coverTests, disjointTests, synthesizeTests}
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast.BrboExprUtils.{greaterThan, greaterThanOrEqualTo}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.string.StringCompare
import brbo.common.{BrboType, CommandLineArguments, Z3Solver}
import org.scalatest.flatspec.AnyFlatSpec

/*class SynthesizerUnitTest extends AnyFlatSpec {
  "Synthesizing programs" should "succeed" in {
    val programSynthesis = new Synthesizer(SynthesizerUnitTest.program, CommandLineArguments.TEST_ARGUMENTS)
    synthesizeTests.foreach({
      testCase =>
        val newFunction = programSynthesis.synthesize(testCase.input.asInstanceOf[Refinement]).mainFunction
        assert(StringCompare.ignoreWhitespaces(newFunction.toIR(), testCase.expectedOutput, s"Test case `${testCase.name}` failed!"))
    })
  }

  "Deciding disjointness" should "be correct" in {
    disjointTests.foreach({
      testCase =>
        val predicates = testCase.input.asInstanceOf[Iterable[Predicate]]
        val solver = new Z3Solver
        assert(StringCompare.ignoreWhitespaces(Synthesizer.isDisjoint(predicates, solver).toString, testCase.expectedOutput, s"Test case `${testCase.name}` failed!"))
    })
  }

  "Deciding covers" should "be correct" in {
    coverTests.foreach({
      testCase =>
        val predicates = testCase.input.asInstanceOf[Iterable[Predicate]]
        val solver = new Z3Solver
        assert(StringCompare.ignoreWhitespaces(Synthesizer.isCover(predicates, solver).toString, testCase.expectedOutput, s"Test case `${testCase.name}` failed!"))
    })
  }
}*/

object SynthesizerUnitTest {
  private val i: Identifier = Identifier("i", INT)
  private val n: Identifier = Identifier("n", INT)

  private val reset: Reset = Reset(1)
  private val use: Use = Use(Some(1), Number(1), greaterThanOrEqualTo(n, Number(0)))
  private val declaration = VariableDeclaration(i, Number(0))
  private val increment = Assignment(i, Addition(i, Number(1)))
  private val condition = LessThan(i, n)
  private val loop = Loop(condition, Block(List(reset, use, increment)))

  private val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n), Block(List(declaration, loop)), Set(1))
  private val program: BrboProgram = BrboProgram("Test program", packageName = None, mainFunction)

  val synthesizeTests: List[TestCase] = {
    val path = List(
      CFGNode((VariableDeclaration(reset.resourceVariable, Number(0))), Some(mainFunction), CFGNode.DONT_CARE_ID), // 0
      CFGNode((VariableDeclaration(reset.starVariable, Number(0))), Some(mainFunction), CFGNode.DONT_CARE_ID), // 1
      CFGNode((VariableDeclaration(reset.counterVariable, Number(-1))), Some(mainFunction), CFGNode.DONT_CARE_ID), // 2
      CFGNode((declaration), Some(mainFunction), CFGNode.DONT_CARE_ID), // 3
      CFGNode((condition), Some(mainFunction), CFGNode.DONT_CARE_ID), // 4
      CFGNode((reset), Some(mainFunction), CFGNode.DONT_CARE_ID), // 5
      CFGNode((use), Some(mainFunction), CFGNode.DONT_CARE_ID), // 6
      CFGNode((increment), Some(mainFunction), CFGNode.DONT_CARE_ID), // 7
      CFGNode((condition), Some(mainFunction), CFGNode.DONT_CARE_ID), // 8
      CFGNode((reset), Some(mainFunction), CFGNode.DONT_CARE_ID), // 9
      CFGNode((use), Some(mainFunction), CFGNode.DONT_CARE_ID), // 10
    )

    val splitUsesTest01 = {
      val reset2 = CFGNode((Reset(2)), Some(mainFunction), CFGNode.DONT_CARE_ID)
      val use2 = CFGNode((Use(Some(2), use.update, use.condition)), Some(mainFunction), CFGNode.DONT_CARE_ID)
      val reset3 = CFGNode((Reset(3)), Some(mainFunction), CFGNode.DONT_CARE_ID)
      val use3 = CFGNode((Use(Some(3), use.update, use.condition)), Some(mainFunction), CFGNode.DONT_CARE_ID)
      Map(5 -> ResetNode(reset2, 2), 6 -> UseNode(use2, 2), 9 -> ResetNode(reset3, 3), 10 -> UseNode(use3, 3))
    }
    val groupIdsTest01 = Map(1 -> Set(2, 3))

    List(
      TestCase("Split Uses", Refinement(path, splitUsesTest01, Set(), groupIdsTest01),
        """void main(int n)
          |{
          |  int C2 = -1;
          |  int C3 = -1;
          |  int R2 = 0;
          |  int R3 = 0;
          |  int S2 = 0;
          |  int S3 = 0;
          |  int i = 0;
          |  while (i < n)
          |  {
          |    {
          |      if (true) reset R2
          |      if (true) reset R3
          |    }
          |    {
          |      if (((0 - i) >= 0) && (n >= 0)) use R2 1
          |      if ((i > 0) && (n >= 0)) use R3 1
          |    }
          |    i = i + 1;
          |  }
          |}""".stripMargin),
      TestCase("Remove Resets", Refinement(path, Map(), Set(5), Map()),
        """void main(int n)
          |{
          |  int C1 = -1;
          |  int R1 = 0;
          |  int S1 = 0;
          |  int i = 0;
          |  while (i < n)
          |  {
          |    {
          |      if (true && (i > 0)) reset R1
          |    }
          |    {
          |      if (n >= 0) use R1 1
          |    }
          |    i = i + 1;
          |  }
          |}""".stripMargin),
      TestCase("Split Uses and Remove Resets", Refinement(path, splitUsesTest01, Set(5), groupIdsTest01),
        """void main(int n)
          |{
          |  int C2 = -1;
          |  int C3 = -1;
          |  int R2 = 0;
          |  int R3 = 0;
          |  int S2 = 0;
          |  int S3 = 0;
          |  int i = 0;
          |  while (i < n)
          |  {
          |    {
          |      if (false) reset R2
          |      if (true) reset R3
          |    }
          |    {
          |      if (((0 - i) >= 0) && (n >= 0)) use R2 1
          |      if ((i > 0) && (n >= 0)) use R3 1
          |    }
          |    i = i + 1;
          |  }
          |}""".stripMargin),
    )
  }

  val disjointTests: List[TestCase] = {
    val i = Identifier("i", BrboType.INT)
    val n = Identifier("n", BrboType.INT)
    val zero = Number(0)

    val list1 = List(Predicate(greaterThan(zero, i)), Predicate(greaterThan(i, zero)))
    val list2 = List(Predicate(greaterThanOrEqualTo(zero, i)), Predicate(greaterThan(i, zero)))
    val list3 = List(Predicate(greaterThanOrEqualTo(i, zero)), Predicate(And(greaterThan(n, zero), greaterThan(zero, i))))
    val list4 = List(Predicate(And(greaterThanOrEqualTo(n, zero), greaterThan(zero, n))), Predicate(Bool(b = true)))
    val list5 = List(Predicate(greaterThanOrEqualTo(zero, i)), Predicate(greaterThanOrEqualTo(i, zero)))
    List(
      TestCase("Disjoint Test 01", list1, """true"""),
      TestCase("Disjoint Test 02", list2, """true"""),
      TestCase("Disjoint Test 03", list3, """true"""),
      TestCase("Disjoint Test 04", list4, """true"""),
      TestCase("Disjoint Test 05", list5, """false"""),
    )
  }

  val coverTests: List[TestCase] = {
    val i = Identifier("i", BrboType.INT)
    val n = Identifier("n", BrboType.INT)
    val zero = Number(0)

    val list1 = List(Predicate(greaterThan(zero, n)), Predicate(greaterThanOrEqualTo(n, zero)))
    val list2 = List(Predicate(greaterThanOrEqualTo(zero, n)), Predicate(greaterThanOrEqualTo(n, zero)))
    val list3 = List(Predicate(greaterThanOrEqualTo(i, n)), Predicate(greaterThanOrEqualTo(n, zero)))
    List(
      TestCase("Cover Test 01", list1, """true"""),
      TestCase("Cover Test 02", list2, """true"""),
      TestCase("Cover Test 03", list3, """false"""),
    )
  }
}
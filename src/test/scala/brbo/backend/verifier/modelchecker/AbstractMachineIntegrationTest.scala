package brbo.backend.verifier.modelchecker

import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.modelchecker.AbstractDomainName._
import brbo.common.BrboType._
import brbo.common.CommandLineArguments._
import brbo.common.ast._
import brbo.common.string.StringCompare
import brbo.common.{BrboType, _}
import org.scalatest.flatspec.AnyFlatSpec

class AbstractMachineIntegrationTest extends AnyFlatSpec {
  private val n: Identifier = Identifier("n", INT)
  private val a: Identifier = Identifier("a", INT)
  private val t = Identifier("t", BrboType.INT)
  private val groupId = 1
  private val approximatedResourceUsage = GhostVariableUtils.generateSum(Some(groupId))

  private val program1: BrboProgram = {
    val i: Identifier = Identifier("i", INT)
    val reset: Reset = Reset(groupId)
    val use: Use = Use(Some(groupId), a)
    val declaration = VariableDeclaration(i, Number(0))
    val increment = Assignment(i, Addition(i, Number(1)))
    val condition = LessThan(i, n)
    val assume = Assume(And(GreaterThan(a, Number(0)), GreaterThan(n, Number(0))))
    val loop = Loop(condition, Block(List(reset, use, increment)))
    val mainFunction = BrboFunction("main", VOID, List(n, a), Block(List(assume, declaration, loop)), Set(groupId))
    BrboProgram("Test program 1", mainFunction)
  }

  private val (program2, program3) = {
    val assume = Assume(GreaterThan(t, Number(0)))
    val l = Identifier("l", BrboType.INT)
    val s = Identifier("s", BrboType.INT)
    val e = Identifier("e", BrboType.INT)
    val reset = Reset(groupId)
    val s1 = Assume(And(LessThanOrEqualTo(e, s), LessThanOrEqualTo(s, t)))
    val s2 = Assume(And(LessThanOrEqualTo(s, e), LessThanOrEqualTo(e, t)))
    val use1 = Use(Some(groupId), Subtraction(s, l))
    val s3 = Assignment(l, e)

    val loop1 = Loop(LessThan(e, t), Block(List(s1, s2, reset, use1, s3)))
    val use2 = Use(Some(groupId), Subtraction(t, l))
    val mainFunction1 = BrboFunction("main", VOID, List(t),
      Block(List(assume, VariableDeclaration(l, Number(0)), VariableDeclaration(s, Number(0)),
        VariableDeclaration(e, Number(0)), loop1, use2)), Set(groupId))
    val program1 = BrboProgram("Test program 2", mainFunction1)

    val loop2 = Loop(LessThan(e, t), Block(List(s1, s2, use1, s3)))
    val mainFunction2 = BrboFunction("main", VOID, List(t),
      Block(List(assume, VariableDeclaration(l, Number(0)), VariableDeclaration(s, Number(0)), reset,
        VariableDeclaration(e, Number(0)), loop2, use2)), Set(groupId))
    val program2 = BrboProgram("Test program 2", mainFunction2)
    (program1, program2)
  }

  private val maxPathLength = 30
  private val checkWithZ3 = true
  private val debugMode = true
  private val polkaArgument: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode, "",
      printVerifierInputs = false, verifierTimeout = DEFAULT_TIMEOUT, printCFG = false,
      maxGroups = DEFAULT_MAX_GROUPS,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false, DEFAULT_MAX_ITERATIONS, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = POLKA_STRICT.toString, maxPathLength, checkWithZ3)
    arguments
  }

  "Model checking program 1" should "be correct" in {
    val abstractMachine = new AbstractMachine(program1, polkaArgument)
    val assertion = LessThanOrEqualTo(approximatedResourceUsage, Multiplication(n, a))
    val result = abstractMachine.verify(assertion).result
    StringCompare.ignoreWhitespaces(result.toString, """VerifierResult(TRUE_RESULT,Set())""".stripMargin)
  }

  // TODO: Uncomment and debug these tests
  /*"Model checking program 2" should "be correct" in {
    val abstractMachine = new AbstractMachine(program2, polkaArgument)
    val assertion = LessThanOrEqualTo(approximatedResourceUsage, t)
    val result = abstractMachine.verify(assertion).result
    StringCompare.ignoreWhitespaces(result.toString,
      """VerifierResult(UNKNOWN_RESULT,Set(Path:
        |  [000] (2) int C1 = -1; [fun `main`]
        |  [001] (3) int R1 = 0; [fun `main`]
        |  [002] (4) int S1 = 0; [fun `main`]
        |  [003] (5) assume(t > 0); [fun `main`]
        |  [004] (6) int l = 0; [fun `main`]
        |  [005] (7) int s = 0; [fun `main`]
        |  [006] (8) int e = 0; [fun `main`]
        |  [007] (9) [Branch Head] [fun `main`]
        |  [008] (10) (e < t) [fun `main`]
        |  [009] (13) assume((e <= s) && (s <= t)); [fun `main`]
        |  [010] (14) assume((s <= e) && (e <= t)); [fun `main`]
        |  [011] (15) if (true) reset R1 [fun `main`]
        |  [012] (16) if (true) use R1 (s - l) [fun `main`]
        |  [013] (17) l = e; [fun `main`]
        |  [014] (10) (e < t) [fun `main`]
        |  [015] (13) assume((e <= s) && (s <= t)); [fun `main`]
        |  [016] (14) assume((s <= e) && (e <= t)); [fun `main`]
        |  [017] (15) if (true) reset R1 [fun `main`]
        |  [018] (16) if (true) use R1 (s - l) [fun `main`]
        |  [019] (17) l = e; [fun `main`]
        |  [020] (10) (e < t) [fun `main`]
        |  [021] (13) assume((e <= s) && (s <= t)); [fun `main`]
        |  [022] (14) assume((s <= e) && (e <= t)); [fun `main`]
        |  [023] (15) if (true) reset R1 [fun `main`]
        |  [024] (16) if (true) use R1 (s - l) [fun `main`]
        |  [025] (17) l = e; [fun `main`]
        |  [026] (10) (e < t) [fun `main`]
        |  [027] (13) assume((e <= s) && (s <= t)); [fun `main`]
        |  [028] (14) assume((s <= e) && (e <= t)); [fun `main`]
        |  [029] (15) if (true) reset R1 [fun `main`]
        |  [030] (16) if (true) use R1 (s - l) [fun `main`]))""".stripMargin)
  }

  "Model checking program 3" should "be correct" in {
    val abstractMachine = new AbstractMachine(program3, polkaArgument)
    val assertion = LessThanOrEqualTo(approximatedResourceUsage, Multiplication(n, a))
    val result = abstractMachine.verify(assertion).result
    StringCompare.ignoreWhitespaces(result.toString, """VerifierResult(TRUE_RESULT,Set())""".stripMargin)
  }*/
}

object AbstractMachineIntegrationTest {
}

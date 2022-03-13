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
  private val resourceVariable = GhostVariableUtils.generateVariable(Some(groupId), GhostVariableTyp.Resource)

  private val program1: BrboProgram = {
    val i: Identifier = Identifier("i", INT)
    val reset: Reset = Reset(groupId)
    val use: Use = Use(Some(groupId), a, GreaterThanOrEqualTo(n, Number(0)))
    val declaration = VariableDeclaration(i, Number(0))
    val increment = Assignment(i, Addition(i, Number(1)))
    val condition = LessThan(i, n)
    val assume = Assume(GreaterThan(a, Number(0)))
    val loop = Loop(condition, Block(List(reset, use, increment)))
    val mainFunction = BrboFunction("main", VOID, List(n, a), Block(List(assume, declaration, loop)), Set(groupId))
    BrboProgram("Test program 1", mainFunction)
  }

  private val program2: BrboProgram = {
    val assume = Assume(GreaterThan(t, Number(0)))
    val l = Identifier("l", BrboType.INT)
    val s = Identifier("s", BrboType.INT)
    val e = Identifier("e", BrboType.INT)
    val loop = {
      val s1 = Assume(And(LessThanOrEqualTo(e, s), LessThanOrEqualTo(s, t)))
      // Assignment(s, FunctionCallExpr("ndInt2", List(Addition(e, Number(1)), t), BrboType.INT))
      val s2 = Assume(And(LessThanOrEqualTo(s, e), LessThanOrEqualTo(e, t)))
      // Assignment(e, FunctionCallExpr("ndInt2", List(Addition(s, Number(1)), t), BrboType.INT))
      val reset = Reset(groupId)
      val use = Use(Some(groupId), Subtraction(s, l))
      val s3 = Assignment(l, e)
      Loop(LessThan(e, t), Block(List(s1, s2, reset, use, s3)))
    }
    val use = Use(Some(groupId), Subtraction(t, l))
    val mainFunction = BrboFunction("main", VOID, List(t),
      Block(List(assume, VariableDeclaration(l, Number(0)), VariableDeclaration(s, Number(0)),
        VariableDeclaration(e, Number(0)), loop, use)), Set(groupId))
    BrboProgram("Test program 2", mainFunction)
  }

  private val program3: BrboProgram = {
    val assume = Assume(GreaterThan(t, Number(0)))
    val reset = Reset(groupId)
    val l = Identifier("l", BrboType.INT)
    val s = Identifier("s", BrboType.INT)
    val e = Identifier("e", BrboType.INT)
    val loop = {
      val s1 = Assume(And(LessThanOrEqualTo(e, s), LessThanOrEqualTo(s, t)))
      val s2 = Assume(And(LessThanOrEqualTo(s, e), LessThanOrEqualTo(e, t)))
      val use = Use(Some(groupId), Subtraction(s, l))
      val s3 = Assignment(l, e)
      Loop(LessThan(e, t), Block(List(s1, s2, use, s3)))
    }
    val use = Use(Some(groupId), Subtraction(t, l))
    val mainFunction = BrboFunction("main", VOID, List(t),
      Block(List(assume, VariableDeclaration(l, Number(0)), VariableDeclaration(s, Number(0)), reset,
        VariableDeclaration(e, Number(0)), loop, use)), Set(groupId))
    BrboProgram("Test program 2", mainFunction)
  }

  private val maxPathLength = 30
  private val checkWithZ3 = true
  private val debugMode = false
  private val octagonArgument: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode, "",
      printVerifierInputs = false, verifierTimeout = DEFAULT_TIMEOUT, printCFG = false,
      maxGroups = DEFAULT_MAX_GROUPS,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false, DEFAULT_MAX_ITERATIONS, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = OCTAGON.toString, maxPathLength, checkWithZ3)
    arguments
  }
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
    val assertion = LessThanOrEqualTo(resourceVariable, Multiplication(n, a))
    val result = abstractMachine.verify(assertion).result
    StringCompare.ignoreWhitespaces(result.toString, """VerifierResult(TRUE_RESULT,Set())""".stripMargin)
  }

  "Model checking program 2" should "be correct" in {
    val abstractMachine = new AbstractMachine(program2, polkaArgument)
    val assertion = LessThanOrEqualTo(resourceVariable, t)
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
        |  [007] (9) (e < t) [fun `main`]
        |  [008] (11) assume((e <= s) && (s <= t)); [fun `main`]
        |  [009] (12) assume((s <= e) && (e <= t)); [fun `main`]
        |  [010] (13) if (true) reset R1 [fun `main`]
        |  [011] (14) if (true) use R1 (s - l) [fun `main`]
        |  [012] (15) l = e; [fun `main`]
        |  [013] (9) (e < t) [fun `main`]
        |  [014] (11) assume((e <= s) && (s <= t)); [fun `main`]
        |  [015] (12) assume((s <= e) && (e <= t)); [fun `main`]
        |  [016] (13) if (true) reset R1 [fun `main`]
        |  [017] (14) if (true) use R1 (s - l) [fun `main`]
        |  [018] (15) l = e; [fun `main`]
        |  [019] (9) (e < t) [fun `main`]
        |  [020] (11) assume((e <= s) && (s <= t)); [fun `main`]
        |  [021] (12) assume((s <= e) && (e <= t)); [fun `main`]
        |  [022] (13) if (true) reset R1 [fun `main`]
        |  [023] (14) if (true) use R1 (s - l) [fun `main`]
        |  [024] (15) l = e; [fun `main`]
        |  [025] (9) (e < t) [fun `main`]
        |  [026] (11) assume((e <= s) && (s <= t)); [fun `main`]
        |  [027] (12) assume((s <= e) && (e <= t)); [fun `main`]
        |  [028] (13) if (true) reset R1 [fun `main`]
        |  [029] (14) if (true) use R1 (s - l) [fun `main`]
        |  [030] (15) l = e; [fun `main`]))""".stripMargin)
  }

  "Model checking program 3" should "be correct" in {
    val abstractMachine = new AbstractMachine(program3, polkaArgument)
    val assertion = LessThanOrEqualTo(resourceVariable, Multiplication(n, a))
    val result = abstractMachine.verify(assertion).result
    StringCompare.ignoreWhitespaces(result.toString, """VerifierResult(TRUE_RESULT,Set())""".stripMargin)
  }
}

object AbstractMachineIntegrationTest {
}

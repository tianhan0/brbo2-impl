package brbo.backend.verifier.modelchecker

import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.modelchecker.AbstractDomainName._
import brbo.common.BrboType._
import brbo.common.{CommandLineArguments, StringCompare}
import brbo.common.CommandLineArguments._
import brbo.common.ast._
import org.scalatest.flatspec.AnyFlatSpec

class AbstractMachineIntegrationTest extends AnyFlatSpec {
  private val i: Identifier = Identifier("i", INT)
  private val n: Identifier = Identifier("n", INT)
  private val a: Identifier = Identifier("a", INT)

  private val reset: Reset = Reset(1)
  private val use: Use = Use(Some(1), a, GreaterThanOrEqualTo(n, Number(0)))
  private val declaration = VariableDeclaration(i, Number(0))
  private val increment = Assignment(i, Addition(i, Number(1)))
  private val condition = LessThan(i, n)
  private val assume = Assume(GreaterThan(a, Number(0)))
  private val loop = Loop(condition, Block(List(reset, use, increment)))

  private val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n, a), Block(List(assume, declaration, loop)), Set(1))
  private val program: BrboProgram = BrboProgram("Test program", mainFunction)

  private val maxIterations = 100
  private val checkWithZ3 = true
  private val debugMode = false
  private val octagonArgument: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode, "",
      printVerifierInputs = false, verifierTimeout = DEFAULT_TIMEOUT, printCFG = false,
      generateSynthetic = 0, maxGroups = DEFAULT_MAX_GROUPS,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false, maxIterations, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = OCTAGON.toString, maxPathLength = DEFAULT_MAX_PATH_LENGTH, checkWithZ3)
    arguments
  }
  private val polkaArgument: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode, "",
      printVerifierInputs = false, verifierTimeout = DEFAULT_TIMEOUT, printCFG = false,
      generateSynthetic = 0, maxGroups = DEFAULT_MAX_GROUPS,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false, maxIterations, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = POLKA_STRICT.toString, maxPathLength = DEFAULT_MAX_PATH_LENGTH, checkWithZ3)
    arguments
  }

  "Model checking programs" should "be correct" in {
    val abstractMachine = new AbstractMachine(program, polkaArgument)
    val assertion = LessThanOrEqualTo(reset.resourceVariable, Multiplication(n, a))
    val result = abstractMachine.verify(assertion)
    StringCompare.ignoreWhitespaces(result.toString, """VerifierResult(TRUE_RESULT,Set())""".stripMargin)
  }
}

object AbstractMachineIntegrationTest {
}

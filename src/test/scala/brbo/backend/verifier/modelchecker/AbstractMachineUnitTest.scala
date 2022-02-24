package brbo.backend.verifier.modelchecker

import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.common.BrboType._
import brbo.common.CommandLineArguments
import brbo.common.CommandLineArguments._
import brbo.common.ast._
import org.scalatest.flatspec.AnyFlatSpec

class AbstractMachineUnitTest extends AnyFlatSpec {
  val octagonArguments: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode = false, "", skipSanityCheck = false,
      printVerifierInputs = false, verifierTimeout = DEFAULT_TIMEOUT, printCFG = false,
      generateSynthetic = 0, maxGroups = DEFAULT_MAX_GROUPS,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false, maxIterations = DEFAULT_MAX_ITERATIONS, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = DEFAULT_ABSTRACT_DOMAIN, maxPathLength = DEFAULT_MAX_PATH_LENGTH)
    arguments
  }

  "Operations over Valuation" should "be correct" in {
    // TODO
  }
}

object AbstractMachineUnitTest {
  private val i: Identifier = Identifier("i", INT)
  private val n: Identifier = Identifier("n", INT)

  private val reset: Reset = Reset(1)
  private val use: Use = Use(Some(1), Number(1), GreaterThanOrEqualTo(n, Number(0)))
  private val declaration = VariableDeclaration(i, Number(0))
  private val increment = Assignment(i, Addition(i, Number(1)))
  private val condition = LessThan(i, n)
  private val loop = Loop(condition, Block(List(reset, use, increment)))

  private val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n), Block(List(declaration, loop)), Set(1))
  private val program: BrboProgram = BrboProgram("Test program", mainFunction)
}

// Under Octagon / Polka:
// pre-condition: r* <= a
// command: r* = r* + a
// post-condition: ???

// Under Octagon / Polka:
// pre-condition: r* <= a
// command: r* = b
// post-condition: ???

// Under Octagon / Polka:
// pre-condition: r <= a
// command: r = r + 1
// post-condition: ???
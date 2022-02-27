package brbo.backend.verifier.modelchecker

import apron.Octagon
import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.Apron.{Conjunction, Disjunction, Singleton}
import brbo.common.BrboType._
import brbo.common.CommandLineArguments._
import brbo.common.ast._
import brbo.common.{BrboType, CommandLineArguments, MyLogger, StringCompare}
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
  private val logger = Some(MyLogger.createLogger(classOf[AbstractMachineUnitTest], debugMode = false))
  private val x = Identifier("x", BrboType.INT)
  private val y = Identifier("y", BrboType.BOOL)
  private val z = Identifier("z", BrboType.INT)

  "Creating new variables in a valuation" should "be correct" in {
    val v1 = AbstractMachine.createEmptyValuation(new Octagon()).createUninitializedNewVariable(Variable(x, None))
    StringCompare.ignoreWhitespaces(v1.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <universal>""".stripMargin, "Variables: x (uninitialized)")
    val v2 = v1.createNewVariable(Variable(x, None))
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 >= 0;  -1x0 >= 0 }""".stripMargin, "Variables: x (same scope)")
    val v3 = {
      try {
        v2.createUninitializedNewVariable(Variable(x, None))
      }
      catch {
        case _: Exception => v1
      }
    }
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <universal>""".stripMargin, "The catch case should be exercised")
    val v4 = v2.createNewVariable(Variable(y, None))
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(y,None)
        |  Variable(x,None)
        |ApronState: {  1x0 >= 0;  -1x0 >= 0;  -1x0 +1x1 >= 0;  1x0 +1x1 >= 0;  1x1 >= 0;  -1x0 -1x1 >= 0;  1x0 -1x1 >= 0;  -1x1 >= 0 }""".stripMargin, "Variables: x (same scope), y")
    val v5 = v2.createNewVariable(Variable(x, Some(Block(List(Skip())))))
    StringCompare.ignoreWhitespaces(v5.toString,
      """Variables:
        |  Variable(x,Some({
        |  ;
        |}))
        |  Variable(x,None)
        |ApronState: {  1x0 >= 0;  -1x0 >= 0;  -1x0 +1x1 >= 0;  1x0 +1x1 >= 0;  1x1 >= 0;  -1x0 -1x1 >= 0;  1x0 -1x1 >= 0;  -1x1 >= 0 }""".stripMargin, "Variables: x, x (different scope)")
  }

  "Assigning to variables in a valuation" should "be correct" in {
    val v1 = AbstractMachine.createEmptyValuation(new Octagon())
    val v2 = v1.createNewVariable(Variable(x, None))
    val v3 = v2.assignCopy(Variable(x, None), Apron.mkIntVal(10))
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -10.0 >= 0;  -1x0 +10.0 >= 0 }""".stripMargin)
    val v4 = v3.assignCopy(Variable(x, None), Apron.mkIntVal(11))
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -11.0 >= 0;  -1x0 +11.0 >= 0 }""".stripMargin)
    val v5 = v4.assignCopy(Variable(y, None), Apron.mkIntVal(12))
    StringCompare.ignoreWhitespaces(v5.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -11.0 >= 0;  -1x0 +11.0 >= 0 }""".stripMargin)
  }

  "Joining two valuations" should "be correct" in {
    val v1 = AbstractMachine.createEmptyValuation(new Octagon()).createNewVariable(Variable(x, None))
    val v2 = v1.assignCopy(Variable(x, None), Apron.mkIntVal(50))
    val v3 = v1.assignCopy(Variable(x, None), Apron.mkIntVal(100))
    val v4 = v2.joinCopy(v3)
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -50.0 >= 0;  -1x0 +100.0 >= 0 }""".stripMargin)
  }

  "Imposing constraints on a valuation" should "be correct" in {
    val v1 = AbstractMachine.createEmptyValuation(new Octagon()).createUninitializedNewVariable(Variable(x, None))
    val v2 = {
      val ge = Apron.mkGt(Apron.mkVar(0), Apron.mkIntVal(5))
      val le = Apron.mkGe(Apron.mkIntVal(10), Apron.mkVar(0))
      v1.imposeConstraint(Conjunction(Singleton(ge), Singleton(le)))
    }
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -5.0 >= 0;  -1x0 +10.0 >= 0 }""".stripMargin)
    val v3 = {
      val eq1 = Apron.mkLt(Apron.mkVar(0), Apron.mkIntVal(100))
      val eq2 = Apron.mkLe(Apron.mkIntVal(50), Apron.mkVar(0))
      v1.imposeConstraint(Disjunction(Singleton(eq1), Singleton(eq2)))
    }
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -50.0 >= 0;  -1x0 +100.0 >= 0 }""".stripMargin)
  }

  "Finding indices of variables from a valuation" should "be correct" in {
    /**
     * {
     * x, y
     * {
     * x
     * }
     * }
     * {
     * x, y
     * }
     */
    val v1 = AbstractMachine.createEmptyValuation(new Octagon(), None, logger)
    val scope1: Option[Statement] = Some(Block(Nil))
    val scope2: Option[Statement] = Some(Block(Nil))
    val scope3: Option[Statement] = Some(Block(Nil))
    val v2 = v1.createNewVariable(Variable(x, scope1))
    val v3 = v2.createNewVariable(Variable(y, scope2))
    val v4 = v3.createNewVariable(Variable(z, scope1))
  }

  "Inclusion check between two valuations" should "be correct" in {

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
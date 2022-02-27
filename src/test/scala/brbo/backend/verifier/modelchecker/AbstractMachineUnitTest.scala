package brbo.backend.verifier.modelchecker

import apron.{Octagon, Tcons0}
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
  private val emptyValuation = AbstractMachine.createEmptyValuation(new Octagon(), None, logger)

  "Creating new variables in a valuation" should "be correct" in {
    val v1 = emptyValuation.createUninitializedVariable(Variable(x, None))
    StringCompare.ignoreWhitespaces(v1.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <universal>""".stripMargin, "Variables: x (uninitialized)")
    val v2 = v1.createInitializedVariable(Variable(x, None))
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 >= 0;  -1x0 >= 0 }""".stripMargin, "Variables: x (same scope)")
    val v3 = {
      try {
        v2.createUninitializedVariable(Variable(x, None))
      }
      catch {
        case _: Exception => v1
      }
    }
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <universal>""".stripMargin, "The catch case should be exercised")
    val v4 = v2.createInitializedVariable(Variable(y, None))
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(y,None)
        |ApronState: {  1x0 >= 0;  -1x0 >= 0;  -1x0 +1x1 >= 0;  1x0 +1x1 >= 0;  1x1 >= 0;  -1x0 -1x1 >= 0;  1x0 -1x1 >= 0;  -1x1 >= 0 }""".stripMargin, "Variables: x (same scope), y")
    val v5 = v2.createInitializedVariable(Variable(x, Some(Block(List(Skip())))))
    StringCompare.ignoreWhitespaces(v5.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(x,Some({
        |  ;
        |}))
        |ApronState: {  1x0 >= 0;  -1x0 >= 0;  -1x0 +1x1 >= 0;  1x0 +1x1 >= 0;  1x1 >= 0;  -1x0 -1x1 >= 0;  1x0 -1x1 >= 0;  -1x1 >= 0 }""".stripMargin, "Variables: x, x (different scope)")
  }

  "Assigning to variables in a valuation" should "be correct" in {
    val v1 = emptyValuation
    val v2 = v1.createInitializedVariable(Variable(x, None))
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
    val v1 = emptyValuation.createInitializedVariable(Variable(x, None))
    val v2 = v1.assignCopy(Variable(x, None), Apron.mkIntVal(50))
    val v3 = v1.assignCopy(Variable(x, None), Apron.mkIntVal(100))
    val v4 = v2.joinCopy(v3)
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -50.0 >= 0;  -1x0 +100.0 >= 0 }""".stripMargin)
  }

  "Imposing constraints on a valuation" should "be correct" in {
    val xVar = Variable(x, None)
    val v1 = emptyValuation.createUninitializedVariable(xVar)
    val xApronVar = v1.apronVariableAnyScope(xVar)
    val v2 = {
      val gt = Apron.mkGt(xApronVar, Apron.mkIntVal(3)) // x>3 <=> x>=4
      val le = Apron.mkGe(Apron.mkIntVal(10), xApronVar)
      v1.imposeConstraint(Conjunction(Singleton(gt), Singleton(le)))
    }
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -4.0 >= 0;  -1x0 +10.0 >= 0 }""".stripMargin, "Conjunction")
    val v3 = {
      val eq1 = Apron.mkEq(Apron.mkVar(0), Apron.mkIntVal(100))
      val eq2 = Apron.mkEq(Apron.mkIntVal(50), Apron.mkVar(0))
      v1.imposeConstraint(Disjunction(Singleton(eq1), Singleton(eq2)))
    }
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -50.0 >= 0;  -1x0 +100.0 >= 0 }""".stripMargin, "Disjunction")
  }

  "Finding indices of variables from a valuation" should "be correct" in {
    /**
     * { Scope 1
     * ..x, y
     * ..{ Scope 2
     * ....x
     * ..}
     * }
     * { Scope 3
     * ..x, y
     * }
     */
    val s1 = Block(Nil)
    val s2 = Block(Nil)
    val s3 = Block(Nil)
    val scope1: Option[Statement] = Some(s1)
    val scope2: Option[Statement] = Some(s2)
    val scope3: Option[Statement] = Some(s3)
      val lookupScope = {
        node: BrboAstNode =>
          if (node == s2) scope1
          else None
      }
    val v1 = AbstractMachine.createEmptyValuation(new Octagon(), Some(lookupScope), logger)
    val v2 = v1.createInitializedVariable(Variable(x, scope1))
    val v3 = v2.createInitializedVariable(Variable(y, scope1))
    StringCompare.ignoreWhitespaces(v3.indexOfVariableThisScope(Variable(x, scope1)).toString, """0""", "Look up x in Scope 1")

    val v4 = v3.createInitializedVariable(Variable(x, scope2))
    StringCompare.ignoreWhitespaces(v4.indexOfVariableThisScope(Variable(x, scope2)).toString, """2""", "Look up x in Scope 2")
    StringCompare.ignoreWhitespaces(v4.indexOfVariableThisScope(Variable(y, scope2)).toString, """-1""", "Look up y in Scope 2")
    StringCompare.ignoreWhitespaces(v4.indexOfVariableAnyScope(Variable(y, scope1)).toString, """1""", "Look up y in Scope 1")

    val v5 = v4.createInitializedVariable(Variable(x, scope3))
    val v6 = v5.createInitializedVariable(Variable(y, scope3))
    StringCompare.ignoreWhitespaces(v6.indexOfVariableThisScope(Variable(x, scope3)).toString, """3""", "Look up x in Scope 3")
    StringCompare.ignoreWhitespaces(v6.indexOfVariableThisScope(Variable(x, scope1)).toString, """0""", "Look up x in Scope 1 (again)")
  }

  "Inclusion check between two valuations" should "be correct" in {
    val xVar = Variable(x, None)
    val v1 = emptyValuation.createInitializedVariable(xVar)
    val xApronVar = v1.apronVariableAnyScope(xVar)
    val v2 = v1.imposeConstraint(Singleton(Apron.mkEq(xApronVar, Apron.mkIntVal(10))))
    val v3 = v1.imposeConstraint(Singleton(Apron.mkGe(xApronVar, Apron.mkIntVal(0))))
    StringCompare.compareLiteral(v2.include(v2).toString, "true")
    StringCompare.compareLiteral(v3.include(v2).toString, "true") // x>=0 include x=10
    StringCompare.compareLiteral(v2.include(v3).toString, "false")
  }

  "Check the satisfaction of constraints" should "be correct" in {
    // val xFloat = Identifier("x", BrboType.FLOAT)
    // val xVar = Variable(xFloat, None)
    val xVar = Variable(x, None)
    val v1 = emptyValuation.createUninitializedVariable(xVar)
    val xApronVar = v1.apronVariableAnyScope(xVar)
    val xValue = 10

    val v2 = v1.imposeConstraint(Singleton(Apron.mkEq(xApronVar, Apron.mkIntVal(xValue))))
    StringCompare.ignoreWhitespaces(v2.toString, """Variables:
                                                   |  Variable(x,None)
                                                   |ApronState: {  1x0 -10.0 >= 0;  -1x0 +10.0 >= 0 }""".stripMargin)

    StringCompare.compareLiteral(v2.satisfy(GreaterThanOrEqualTo(x, Number(10)), None).toString, "true", "Satisfy BrboExpr: true")
    StringCompare.compareLiteral(v2.satisfy(GreaterThan(x, Number(100)), None).toString, "false", "Satisfy BrboExpr: false")

    val eq: Tcons0 = Apron.mkEq(xApronVar, Apron.mkIntVal(xValue))
    val gt = Apron.mkGt(xApronVar, Apron.mkIntVal(100))
    StringCompare.compareLiteral(v2.satisfy(eq).toString, "true", "Satisfy Tcons: Eq")
    StringCompare.compareLiteral(v2.satisfy(gt).toString, "false", "Satisfy Tcons: Gt")

    StringCompare.compareLiteral(v2.satisfy(Singleton(eq)).toString, "true", "Satisfy Singleton: Eq")
    StringCompare.compareLiteral(v2.satisfy(Singleton(gt)).toString, "false", "Satisfy Singleton: Gt")
    StringCompare.compareLiteral(v2.satisfy(Singleton(Apron.mkBoolVal(true))).toString, "true", "Satisfy Singleton: true")
    StringCompare.compareLiteral(v2.satisfy(Singleton(Apron.mkBoolVal(false))).toString, "false", "Satisfy Singleton: false")

    val conjunction = {
      val ge = Apron.mkGe(xApronVar, Apron.mkIntVal(10))
      val lt = Apron.mkLt(xApronVar, Apron.mkDoubleVal(10.5))
      Conjunction(Singleton(ge), Singleton(lt))
    }
    StringCompare.compareLiteral(v2.satisfy(conjunction).toString, "true", "Satisfy Conjunction")

    val disjunction = {
      val gt = Apron.mkGt(xApronVar, Apron.mkIntVal(100))
      val lt = Apron.mkLt(xApronVar, Apron.mkIntVal(11))
      Disjunction(Singleton(gt), Singleton(lt))
    }
    StringCompare.compareLiteral(v2.satisfy(disjunction).toString, "true", "Satisfy Disjunction")
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
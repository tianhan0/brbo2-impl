package brbo.backend.verifier.modelchecker

import apron.{Octagon, Polka, Tcons0}
import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.Apron.{Conjunction, Disjunction, Singleton}
import brbo.common.BrboType._
import brbo.common.ast._
import brbo.common.{BrboType, MyLogger, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class AbstractMachineUnitTest extends AnyFlatSpec {
  private val logger = Some(MyLogger.createLogger(classOf[AbstractMachineUnitTest], debugMode = false))
  private val x = Identifier("x", BrboType.INT)
  private val y = Identifier("y", BrboType.BOOL)
  private val z = Identifier("z", BrboType.INT)
  private val a = Identifier("a", BrboType.INT)
  private val b = Identifier("b", BrboType.INT)
  private val emptyValuationOctagon = AbstractMachine.createEmptyValuation(new Octagon(), None, logger)
  private val emptyValuationPolka = AbstractMachine.createEmptyValuation(new Polka(false), None, logger)

  "Transforming a valuation to bottom" should "be correct" in {
    val v1 = emptyValuationOctagon.createUninitializedVariable(Variable(x, None))
    val bottom = v1.toBottom()
    StringCompare.ignoreWhitespaces(bottom.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <empty>""".stripMargin)
    StringCompare.compareLiteral(bottom.apronState.isBottom(bottom.apronState.getCreationManager).toString, "true")
  }

  "Creating new variables in a valuation" should "be correct" in {
    val v1 = emptyValuationOctagon.createUninitializedVariable(Variable(x, None))
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
    val xVar = Variable(x, None)
    val yVar = Variable(y, None)
    val zVar = Variable(z, None)
    val v1 = emptyValuationPolka
    val v2 = v1.createInitializedVariable(xVar).createUninitializedVariable(zVar)
    val xApronVar = v2.apronVariableAnyScope(xVar)
    val zApronVar = v2.apronVariableAnyScope(zVar)
    val v3 = v2.assignCopy(xVar, Apron.mkIntVal(10))
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x0 -10 = 0 }""".stripMargin, "x = 10")
    val v4 = v3.assignCopy(xVar, Apron.mkIntVal(11))
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x0 -11 = 0 }""".stripMargin, "x = 11")
    val v5 = v4.assignCopy(yVar, Apron.mkIntVal(12))
    StringCompare.ignoreWhitespaces(v5.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x0 -11 = 0 }""".stripMargin, "x = 11 (no y)")
    val v6 = v5.assignCopy(xVar, Apron.mkAdd(xApronVar, xApronVar))
    StringCompare.ignoreWhitespaces(v6.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x0 -22 = 0 }""".stripMargin, "When x = 11, do x := x + x")
    val v7 = v6.assignCopy(xVar, Apron.mkAdd(zApronVar, zApronVar))
    StringCompare.ignoreWhitespaces(v7.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  -1x0 +2x1 = 0 }""".stripMargin, "When x = 11, do x := z + z")
  }

  "Joining two valuations" should "be correct" in {
    val v1 = emptyValuationOctagon.createInitializedVariable(Variable(x, None))
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
    val v1 = emptyValuationOctagon.createUninitializedVariable(xVar)
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
    val v1 = emptyValuationOctagon.createInitializedVariable(xVar)
    val xApronVar = v1.apronVariableAnyScope(xVar)
    val v2 = v1.imposeConstraint(Singleton(Apron.mkEq(xApronVar, Apron.mkIntVal(10))))
    val v3 = v1.imposeConstraint(Singleton(Apron.mkGe(xApronVar, Apron.mkIntVal(0))))
    StringCompare.compareLiteral(v2.include(v2).toString, "true")
    StringCompare.compareLiteral(v3.include(v2).toString, "true") // x>=0 include x=10
    StringCompare.compareLiteral(v2.include(v3).toString, "false")
  }

  "Check the satisfaction of constraints for valuations" should "be correct" in {
    // val xFloat = Identifier("x", BrboType.FLOAT)
    // val xVar = Variable(xFloat, None)
    val xVar = Variable(x, None)
    val v1 = emptyValuationOctagon.createUninitializedVariable(xVar)
    val xApronVar = v1.apronVariableAnyScope(xVar)
    val xValue = 10

    val v2 = v1.imposeConstraint(Singleton(Apron.mkEq(xApronVar, Apron.mkIntVal(xValue))))
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -10.0 >= 0;  -1x0 +10.0 >= 0 }""".stripMargin)

    StringCompare.compareLiteral(v2.satisfy(GreaterThanOrEqualTo(x, Number(10))).toString, "true", "Satisfy BrboExpr: true")
    StringCompare.compareLiteral(v2.satisfy(GreaterThan(x, Number(100))).toString, "false", "Satisfy BrboExpr: false")

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

  "Evaluating commands VariableDeclaration and Assignment" should "be correct" in {
    val r = Identifier("r", BrboType.INT)
    val rStar = Identifier("rStar", BrboType.INT)
    val rCounter = Identifier("rCounter", BrboType.INT)
    val rVar = Variable(r, None)
    val rStarVar = Variable(rStar, None)
    val rCounterVar = Variable(rCounter, None)
    val aVar = Variable(a, None)
    val bVar = Variable(b, None)

    // Declare inputs: a, b
    def declareInputs(v: Valuation): Valuation = {
      val v0 = v.createUninitializedVariable(aVar).createUninitializedVariable(bVar)
      val aApronVar = v0.apronVariableAnyScope(aVar)
      val bApronVar = v0.apronVariableAnyScope(bVar)
      v0.imposeConstraint(Conjunction(Singleton(Apron.mkGtZero(aApronVar)), Singleton(Apron.mkGtZero(bApronVar))))
    }

    val v0Polka = declareInputs(emptyValuationPolka)
    StringCompare.ignoreWhitespaces(v0Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |ApronState: {  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "Initialize inputs (Polka)")

    val v0Octagon = declareInputs(emptyValuationOctagon)
    StringCompare.ignoreWhitespaces(v0Octagon.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |ApronState: {  1x0 -1.0 >= 0;  1x0 +1x1 -2.0 >= 0;  1x1 -1.0 >= 0 }""".stripMargin, "Initialize inputs (Octagon)")

    // Initialize r, r*, r#
    def initializeGhostVariables(v: Valuation): Valuation = {
      val v1 = AbstractMachine.evalCommand(v, VariableDeclaration(r, Number(0)), None)
      val v2 = AbstractMachine.evalCommand(v1, VariableDeclaration(rStar, Number(0)), None)
      AbstractMachine.evalCommand(v2, VariableDeclaration(rCounter, Number(-1)), None)
    }

    val v1Polka = initializeGhostVariables(v0Polka)
    StringCompare.ignoreWhitespaces(v1Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(r,None)
        |  Variable(rStar,None)
        |  Variable(rCounter,None)
        |ApronState: {  1x4 +1 = 0;  1x3 = 0;  1x2 = 0;  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "Initialize ghost variables (Polka)")

    val v1Octagon = initializeGhostVariables(v0Octagon)
    val initialization = {
      val rApronVar = v1Octagon.apronVariableAnyScope(rVar)
      val rStarApronVar = v1Octagon.apronVariableAnyScope(rStarVar)
      val rCounterApronVar = v1Octagon.apronVariableAnyScope(rCounterVar)
      val eqR = Singleton(Apron.mkEqZero(rApronVar))
      val eqRStar = Singleton(Apron.mkEqZero(rStarApronVar))
      val eqRCounter = Singleton(Apron.mkEq(rCounterApronVar, Apron.mkIntVal(-1)))
      Apron.createConjunction(List(eqR, eqRStar, eqRCounter))
    }
    StringCompare.ignoreWhitespaces(v1Octagon.satisfy(initialization).toString, """true""".stripMargin, "Initialize ghost variables (Octagon)")

    // r* = r* + a
    def rStarAddA(v: Valuation): Valuation = {
      AbstractMachine.evalCommand(v, Assignment(rStar, Addition(rStar, a)), None)
    }

    val v2Polka = rStarAddA(v1Polka)
    StringCompare.ignoreWhitespaces(v2Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(r,None)
        |  Variable(rStar,None)
        |  Variable(rCounter,None)
        |ApronState: {  -1x0 +1x3 = 0;  1x4 +1 = 0;  1x2 = 0;  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "When r* = 0, do r* = r* + a (Polka)")
    val v2Octagon = rStarAddA(v1Octagon)

    // r* = r* + b
    def rStarAddB(v: Valuation): Valuation = {
      AbstractMachine.evalCommand(v, Assignment(rStar, Addition(rStar, b)), None)
    }

    val v3Polka = rStarAddB(v2Polka)
    StringCompare.ignoreWhitespaces(v3Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(r,None)
        |  Variable(rStar,None)
        |  Variable(rCounter,None)
        |ApronState: {  -1x0 -1x1 +1x3 = 0;  1x4 +1 = 0;  1x2 = 0;  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "When r* = a, do r* = r* + b (Polka)")
    val v3Octagon = rStarAddB(v2Octagon)
    val rStarEqB = {
      val rStarApron = v3Octagon.apronVariableAnyScope(rStarVar)
      val aApron = v3Octagon.apronVariableAnyScope(aVar)
      val bApron = v3Octagon.apronVariableAnyScope(bVar)
      Singleton(Apron.mkEq(rStarApron, Apron.mkAdd(aApron, bApron)))
    }
    StringCompare.compareLiteral(v3Octagon.satisfy(rStarEqB).toString, "false", "When r* = a, do r* = r* + b (Octagon)")

    val v4Polka = rStarAddA(v2Polka)
    StringCompare.ignoreWhitespaces(v4Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(r,None)
        |  Variable(rStar,None)
        |  Variable(rCounter,None)
        |ApronState: {  -2x0 +1x3 = 0;  1x4 +1 = 0;  1x2 = 0;  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "When r* = a, do r* = r* + a (Polka)")
    val v4Octagon = rStarAddA(v2Octagon)
    val rStarEqTwoA = {
      val rStarApron = v3Octagon.apronVariableAnyScope(rStarVar)
      val aApron = v3Octagon.apronVariableAnyScope(aVar)
      Singleton(Apron.mkEq(rStarApron, Apron.mkAdd(aApron, aApron)))
    }
    StringCompare.compareLiteral(v4Octagon.satisfy(rStarEqTwoA).toString, "false", "When r* = a, do r* = r* + a (Octagon)")

    // r = r + a; r = r + 5;
    def rAddAAndFive(v: Valuation): Valuation = {
      val v1 = AbstractMachine.evalCommand(v, Assignment(r, rStar), None)
      AbstractMachine.evalCommand(v1, Assignment(r, Addition(r, Number(5))), None)
    }

    val v5Polka = rAddAAndFive(v2Polka)
    StringCompare.ignoreWhitespaces(v5Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(r,None)
        |  Variable(rStar,None)
        |  Variable(rCounter,None)
        |ApronState: {  -1x0 +1x3 = 0;  -1x0 +1x2 -5 = 0;  1x4 +1 = 0;  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "When r = a, do r = r + 5 (Polka)")
    val v5Octagon = rAddAAndFive(v2Octagon)
    val rEqAPlusFive = {
      val rApron = v3Octagon.apronVariableAnyScope(rVar)
      val aApron = v3Octagon.apronVariableAnyScope(aVar)
      Singleton(Apron.mkEq(rApron, Apron.mkAdd(aApron, Apron.mkIntVal(5))))
    }
    StringCompare.compareLiteral(v5Octagon.satisfy(rEqAPlusFive).toString, "true", "When r = a, do r = r + a (Octagon)")
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
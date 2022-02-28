package brbo.backend.verifier.modelchecker

import apron.{Octagon, Polka, Tcons0}
import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.AbstractMachineUnitTest.{reset, use}
import brbo.backend.verifier.modelchecker.Apron.{Conjunction, Disjunction, Singleton}
import brbo.common.BrboType._
import brbo.common.ast._
import brbo.common.{BrboType, GhostVariableUtils, MyLogger, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class AbstractMachineUnitTest extends AnyFlatSpec {
  private val logger = Some(MyLogger.createLogger(classOf[AbstractMachineUnitTest], debugMode = false))
  private val x = Identifier("x", BrboType.INT)
  private val y = Identifier("y", BrboType.BOOL)
  private val z = Identifier("z", BrboType.INT)
  private val (r, rStar, rCounter) = GhostVariableUtils.generateVariables(Some(1))
  private val a = Identifier("a", BrboType.INT)
  private val b = Identifier("b", BrboType.INT)
  private def emptyValuationOctagon(logger: Option[MyLogger] = None): Valuation = {
    logger match {
      case Some(_) => AbstractMachine.createEmptyValuation(new Octagon(), None, logger)
      case None => AbstractMachine.createEmptyValuation(new Octagon(), None, this.logger)
    }
  }
  private def emptyValuationPolka(logger: Option[MyLogger] = None): Valuation = {
    logger match {
      case Some(_) => AbstractMachine.createEmptyValuation(new Polka(false), None, logger)
      case None => AbstractMachine.createEmptyValuation(new Polka(false), None, this.logger)
    }
  }
  private val xVar = Variable(x, None)
  private val yVar = Variable(y, None)
  private val zVar = Variable(z, None)
  private val rVar = Variable(r, None)
  private val rStarVar = Variable(rStar, None)
  private val rCounterVar = Variable(rCounter, None)
  private val aVar = Variable(a, None)
  private val bVar = Variable(b, None)

  "Transforming a valuation to bottom" should "be correct" in {
    val v1 = emptyValuationOctagon().createUninitializedVariable(xVar)
    val bottom = v1.toBottom()
    StringCompare.ignoreWhitespaces(bottom.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <empty>""".stripMargin)
    StringCompare.compareLiteral(bottom.apronState.isBottom(bottom.apronState.getCreationManager).toString, "true")
  }

  "Creating new variables in a valuation" should "be correct" in {
    val v1 = emptyValuationOctagon().createUninitializedVariable(xVar)
    StringCompare.ignoreWhitespaces(v1.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <universal>""".stripMargin, "Variables: x (uninitialized)")
    val v2 = v1.createInitializedVariable(xVar)
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 >= 0;  -1x0 >= 0 }""".stripMargin, "Variables: x (same scope)")
    val v3 = {
      try {
        v2.createUninitializedVariable(xVar)
      }
      catch {
        case _: Exception => v1
      }
    }
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <universal>""".stripMargin, "The catch case should be exercised")
    val v4 = v2.createInitializedVariable(yVar)
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
    val v1 = emptyValuationPolka()
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
    val v1 = emptyValuationOctagon().createInitializedVariable(Variable(x, None))
    val v2 = v1.assignCopy(xVar, Apron.mkIntVal(50))
    val v3 = v1.assignCopy(xVar, Apron.mkIntVal(100))
    val v4 = v2.joinCopy(v3)
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -50.0 >= 0;  -1x0 +100.0 >= 0 }""".stripMargin)
  }

  "Imposing constraints on a valuation" should "be correct" in {
    val v1 = emptyValuationOctagon().createUninitializedVariable(xVar)
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
      val eq1 = Apron.mkEq(xApronVar, Apron.mkIntVal(100))
      val eq2 = Apron.mkEq(Apron.mkIntVal(50), xApronVar)
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
    val v1 = emptyValuationOctagon().createInitializedVariable(xVar)
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
    val v1Octagon = emptyValuationOctagon().createUninitializedVariable(xVar)
    val xApronVarOctagon = v1Octagon.apronVariableAnyScope(xVar)

    val v1Polka = emptyValuationPolka().createUninitializedVariable(xVar)
    val xApronVarPolka = v1Polka.apronVariableAnyScope(xVar)

    val xValue = 10

    val v2Octagon = v1Octagon.imposeConstraint(Singleton(Apron.mkEq(xApronVarOctagon, Apron.mkIntVal(xValue))))
    StringCompare.ignoreWhitespaces(v2Octagon.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -10.0 >= 0;  -1x0 +10.0 >= 0 }""".stripMargin)

    val v2Polka = v1Polka.imposeConstraint(Singleton(Apron.mkEq(xApronVarPolka, Apron.mkIntVal(xValue))))
    StringCompare.ignoreWhitespaces(v2Polka.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x0 -10 = 0 }""".stripMargin)

    StringCompare.compareLiteral(v2Octagon.satisfy(GreaterThanOrEqualTo(x, Number(10))).toString, "true", "Satisfy BrboExpr: x>=10 (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(GreaterThan(x, Number(100))).toString, "false", "Satisfy BrboExpr: x>100 (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Bool(b = true)).toString, "true", "Satisfy BrboExpr: true (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Bool(b = false)).toString, "false", "Satisfy BrboExpr: false (Octagon)")

    StringCompare.compareLiteral(v2Polka.satisfy(GreaterThanOrEqualTo(x, Number(10))).toString, "true", "Satisfy BrboExpr: x>=10 (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(GreaterThan(x, Number(100))).toString, "false", "Satisfy BrboExpr: x>100 (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Bool(b = true)).toString, "true", "Satisfy BrboExpr: true (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Bool(b = false)).toString, "false", "Satisfy BrboExpr: false (Polka)")

    val eqOctagon: Tcons0 = Apron.mkEq(xApronVarOctagon, Apron.mkIntVal(xValue))
    val gtOctagon = Apron.mkGt(xApronVarOctagon, Apron.mkIntVal(100))
    StringCompare.compareLiteral(v2Octagon.satisfy(eqOctagon).toString, "true", "Satisfy Tcons: Eq (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(gtOctagon).toString, "false", "Satisfy Tcons: Gt (Octagon)")

    val eqPolka: Tcons0 = Apron.mkEq(xApronVarPolka, Apron.mkIntVal(xValue))
    val gtPolka = Apron.mkGt(xApronVarPolka, Apron.mkIntVal(100))
    StringCompare.compareLiteral(v2Polka.satisfy(eqPolka).toString, "true", "Satisfy Tcons: Eq (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(gtPolka).toString, "false", "Satisfy Tcons: Gt (Polka)")

    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(eqOctagon)).toString, "true", "Satisfy Singleton: Eq (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(gtOctagon)).toString, "false", "Satisfy Singleton: Gt (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(Apron.mkBoolVal(true))).toString, "true", "Satisfy Singleton: true (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(Apron.mkBoolVal(false))).toString, "false", "Satisfy Singleton: false (Octagon)")

    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(eqPolka)).toString, "true", "Satisfy Singleton: Eq (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(gtPolka)).toString, "false", "Satisfy Singleton: Gt (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(Apron.mkBoolVal(true))).toString, "true", "Satisfy Singleton: true (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(Apron.mkBoolVal(false))).toString, "false", "Satisfy Singleton: false (Polka)")

    val conjunctionOctagon = {
      val ge = Apron.mkGe(xApronVarOctagon, Apron.mkIntVal(10))
      val lt = Apron.mkLt(xApronVarOctagon, Apron.mkDoubleVal(10.5))
      Conjunction(Singleton(ge), Singleton(lt))
    }
    StringCompare.compareLiteral(v2Octagon.satisfy(conjunctionOctagon).toString, "true", "Satisfy Conjunction (Octagon)")

    val conjunctionPolka = {
      val ge = Apron.mkGe(xApronVarPolka, Apron.mkIntVal(10))
      val lt = Apron.mkLt(xApronVarPolka, Apron.mkIntVal(11))
      Conjunction(Singleton(ge), Singleton(lt))
    }
    println(conjunctionPolka)
    StringCompare.compareLiteral(v2Polka.satisfy(conjunctionPolka).toString, "true", "Satisfy Conjunction (Polka)")

    val disjunctionOctagon = {
      val gt = Apron.mkGt(xApronVarOctagon, Apron.mkIntVal(100))
      val lt = Apron.mkLt(xApronVarOctagon, Apron.mkIntVal(11))
      Disjunction(Singleton(gt), Singleton(lt))
    }
    StringCompare.compareLiteral(v2Octagon.satisfy(disjunctionOctagon).toString, "true", "Satisfy Disjunction (Octagon)")

    val disjunctionPolka = {
      val gt = Apron.mkGt(xApronVarPolka, Apron.mkIntVal(100))
      val lt = Apron.mkLt(xApronVarPolka, Apron.mkIntVal(11))
      Disjunction(Singleton(gt), Singleton(lt))
    }
    println(disjunctionPolka)
    StringCompare.compareLiteral(v2Polka.satisfy(disjunctionPolka).toString, "true", "Satisfy Disjunction (Polka)")
  }

  "Evaluating commands VariableDeclaration and Assignment" should "be correct" in {
    val v0Polka = declareInputsAAndB(emptyValuationPolka())
    StringCompare.ignoreWhitespaces(v0Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |ApronState: {  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "Initialize inputs (Polka)")

    val v0Octagon = declareInputsAAndB(emptyValuationOctagon())
    StringCompare.ignoreWhitespaces(v0Octagon.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |ApronState: {  1x0 -1.0 >= 0;  1x0 +1x1 -2.0 >= 0;  1x1 -1.0 >= 0 }""".stripMargin, "Initialize inputs (Octagon)")

    val v1Polka = initializeGhostVariables(v0Polka)
    StringCompare.ignoreWhitespaces(v1Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
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
      AbstractMachine.evalCommand(v, Assignment(rStar, Addition(rStar, a)), None, logger)
    }

    val v2Polka = rStarAddA(v1Polka)
    StringCompare.ignoreWhitespaces(v2Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1x0 +1x3 = 0;  1x4 +1 = 0;  1x2 = 0;  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "When r* = 0, do r* = r* + a (Polka)")
    val v2Octagon = rStarAddA(v1Octagon)

    // r* = r* + b
    def rStarAddB(v: Valuation): Valuation = {
      AbstractMachine.evalCommand(v, Assignment(rStar, Addition(rStar, b)), None, logger)
    }

    val v3Polka = rStarAddB(v2Polka)
    StringCompare.ignoreWhitespaces(v3Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
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
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
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
      val v1 = AbstractMachine.evalCommand(v, Assignment(r, rStar), None, logger)
      AbstractMachine.evalCommand(v1, Assignment(r, Addition(r, Number(5))), None, logger)
    }

    val v5Polka = rAddAAndFive(v2Polka)
    StringCompare.ignoreWhitespaces(v5Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1x0 +1x3 = 0;  -1x0 +1x2 -5 = 0;  1x4 +1 = 0;  1x1 -1 >= 0;  1x0 -1 >= 0 }""".stripMargin, "When r = a, do r = r + 5 (Polka)")
    val v5Octagon = rAddAAndFive(v2Octagon)
    val rEqAPlusFive = {
      val rApron = v3Octagon.apronVariableAnyScope(rVar)
      val aApron = v3Octagon.apronVariableAnyScope(aVar)
      Singleton(Apron.mkEq(rApron, Apron.mkAdd(aApron, Apron.mkIntVal(5))))
    }
    StringCompare.compareLiteral(v5Octagon.satisfy(rEqAPlusFive).toString, "true", "When r = a, do r = r + a (Octagon)")
  }

  "Evaluating commands Use and Reset" should "be correct" in {
    val logger = Some(MyLogger.createLogger(classOf[AbstractMachineUnitTest], debugMode = true))
    val reset: Reset = Reset(1)
    val use: Use = Use(Some(1), a)
    val v0 = initializeGhostVariables(declareInputsAAndB(emptyValuationPolka()))
    val v1 = {
      // val v1 = AbstractMachine.evalCommand(v0, reset, None, logger)
      // println(s"v1 $v1")
      /*val v2 = AbstractMachine.evalCommand(v1, use, None, logger)
      println(s"v2 $v2")
      val v3 = AbstractMachine.evalCommand(v2, reset, None, logger)
      println(s"v3 $v3")
      AbstractMachine.evalCommand(v3, use, None, logger)*/
    }
    // StringCompare.ignoreWhitespaces(v1.toString, """""", s"$reset; $use; $reset; $use")

    /*val v2 = {
      val v1 = AbstractMachine.evalCommand(v0, reset, None, logger)
      val v2 = AbstractMachine.evalCommand(v1, use, None, logger)
      AbstractMachine.evalCommand(v2, use, None, logger)
    }
    StringCompare.ignoreWhitespaces(v2.toString, """""", s"$reset; $use; $use")*/
  }

  // Initialize r, r*, r#
  private def initializeGhostVariables(v: Valuation): Valuation = {
    val v1 = AbstractMachine.evalCommand(v, VariableDeclaration(r, Number(0)), None)
    val v2 = AbstractMachine.evalCommand(v1, VariableDeclaration(rStar, Number(0)), None)
    AbstractMachine.evalCommand(v2, VariableDeclaration(rCounter, Number(-1)), None)
  }

  // Declare inputs: a, b
  def declareInputsAAndB(v: Valuation): Valuation = {
    val v0 = v.createUninitializedVariable(aVar).createUninitializedVariable(bVar)
    val aApronVar = v0.apronVariableAnyScope(aVar)
    val bApronVar = v0.apronVariableAnyScope(bVar)
    v0.imposeConstraint(Conjunction(Singleton(Apron.mkGtZero(aApronVar)), Singleton(Apron.mkGtZero(bApronVar))))
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
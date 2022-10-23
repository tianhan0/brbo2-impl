package brbo.backend.verifier.modelchecker

import apron.{Octagon, Polka, Tcons0}
import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.Apron.{Conjunction, Disjunction, Singleton}
import brbo.common.ast.BrboExprUtils.{greaterThan, greaterThanOrEqualTo, lessThanOrEqualTo}
import brbo.common.ast._
import brbo.common.string.StringCompare
import brbo.common.{BrboType, GhostVariableUtils, MyLogger}
import org.scalatest.flatspec.AnyFlatSpec

/*class AbstractMachineUnitTest extends AnyFlatSpec {
  private val debugLogger = Some(MyLogger.createLogger(classOf[AbstractMachineUnitTest], debugMode = true))
  private val x = Identifier("x", BrboType.INT)
  private val y = Identifier("y", BrboType.BOOL)
  private val z = Identifier("z", BrboType.INT)
  private val (r, rStar, rCounter) = GhostVariableUtils.generateVariables(Some(1))
  private val a = Identifier("a", BrboType.INT)
  private val b = Identifier("b", BrboType.INT)

  private def emptyValuationOctagon(debug: Boolean): Valuation = {
    if (debug) AbstractMachine.createEmptyValuation(new Octagon(), this.debugLogger)
    else AbstractMachine.createEmptyValuation(new Octagon(), None)
  }

  private def emptyValuationStrictPolka(debug: Boolean): Valuation = {
    if (debug) AbstractMachine.createEmptyValuation(new Polka(true), this.debugLogger)
    else AbstractMachine.createEmptyValuation(new Polka(true), None)
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
    val v1 = emptyValuationOctagon(debug = false).createUninitializedVariable(xVar)
    val bottom = v1.toBottom
    StringCompare.ignoreWhitespaces(bottom.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <empty>""".stripMargin)
    StringCompare.compareLiteral(bottom.apronState.isBottom(bottom.apronState.getCreationManager).toString, "true")

    ApronMemoryManager.releaseMemory()
  }

  "Creating new variables in a valuation" should "be correct" in {
    val v1 = emptyValuationOctagon(debug = false).createUninitializedVariable(xVar)
    StringCompare.ignoreWhitespaces(v1.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: <universal>""".stripMargin, "Variables: x (uninitialized)")
    val v2 = v1.createInitializedVariable(xVar)
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x(00) >= 0;  -1x(00) >= 0 }""".stripMargin, "Variables: x (same scope)")
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
        |ApronState: {  1x(00) >= 0;  -1x(00) >= 0;  -1x(00) +1y(01) >= 0;  1x(00) +1y(01) >= 0;  1y(01) >= 0;  -1x(00) -1y(01) >= 0;  1x(00) -1y(01) >= 0;  -1y(01) >= 0 }""".stripMargin,
      "Variables: x (same scope), y")
    val v5 = v2.createInitializedVariable(Variable(x, Some(Block(List(Skip())))))
    StringCompare.ignoreWhitespaces(v5.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(x,Some({
        |  ;
        |}))
        |ApronState: {  1x(00) >= 0;  -1x(00) >= 0;  -1x(00) +1x(01) >= 0;  1x(00) +1x(01) >= 0;  1x(01) >= 0;  -1x(00) -1x(01) >= 0;  1x(00) -1x(01) >= 0;  -1x(01) >= 0 }""".stripMargin,
      "Variables: x, x (different scope)")

    ApronMemoryManager.releaseMemory()
  }

  "Removing out-of-scope variables from a valuation" should "be correct" in {
    val scope1 = None
    val scope2 = Some(Block(Nil))
    val xVar1 = Variable(x, scope1)
    val zVar1 = Variable(z, scope1)
    val xVar2 = Variable(x, scope2)
    val yVar2 = Variable(y, scope2)
    val v1 = emptyValuationOctagon(debug = false).createUninitializedVariable(xVar1)
      .createUninitializedVariable(zVar1).createUninitializedVariable(xVar2)
    StringCompare.ignoreWhitespaces(v1.removeVariablesInScope(scope2).toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: <universal>""".stripMargin, "Removing uninitialized variables")
    val v2 = v1.createUninitializedVariable(yVar2)
    StringCompare.ignoreWhitespaces(v2.removeVariablesInScope(scope2).toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: <universal>""".stripMargin, "Removing uninitialized variables")
    ApronMemoryManager.releaseMemory()

    val v3 = emptyValuationStrictPolka(debug = false).createInitializedVariable(xVar1)
      .createInitializedVariable(zVar1).createInitializedVariable(xVar2)
    StringCompare.ignoreWhitespaces(v3.removeVariablesInScope(scope2).toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x(02) = 0;  1z(01) = 0;  1x(00) = 0 }""".stripMargin, "Removing initialized variables")
    val v4 = v3.createInitializedVariable(xVar2)
    StringCompare.ignoreWhitespaces(v4.removeVariablesInScope(scope2).toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x(02) = 0;  1z(01) = 0;  1x(00) = 0 }""".stripMargin, "Removing initialized variables")

    ApronMemoryManager.releaseMemory()
  }

  "Assigning to variables in a valuation" should "be correct" in {
    val v1 = emptyValuationStrictPolka(debug = false)
    val v2 = v1.createInitializedVariable(xVar).createUninitializedVariable(zVar)
    val xApronVar = v2.apronVariableAnyScope(xVar)
    val zApronVar = v2.apronVariableAnyScope(zVar)
    val v3 = v2.assignCopy(xVar, Apron.mkIntVal(10))
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x(00) -10 = 0 }""".stripMargin, "x = 10")
    val v4 = v3.assignCopy(xVar, Apron.mkIntVal(11))
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x(00) -11 = 0 }""".stripMargin, "x = 11")
    val v5 = v4.assignCopy(yVar, Apron.mkIntVal(12))
    StringCompare.ignoreWhitespaces(v5.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x(00) -11 = 0 }""".stripMargin, "x = 11 (no y)")
    val v6 = v5.assignCopy(xVar, Apron.mkAdd(xApronVar, xApronVar))
    StringCompare.ignoreWhitespaces(v6.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  1x(00) -22 = 0 }""".stripMargin, "When x = 11, do x := x + x")
    val v7 = v6.assignCopy(xVar, Apron.mkAdd(zApronVar, zApronVar))
    StringCompare.ignoreWhitespaces(v7.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |ApronState: {  -1x(00) +2z(01) = 0 }""".stripMargin, "When x = 11, do x := z + z")

    ApronMemoryManager.releaseMemory()
  }

  "Joining two valuations" should "be correct" in {
    val v1 = emptyValuationOctagon(debug = false).createInitializedVariable(Variable(x, None))
    val v2 = v1.assignCopy(xVar, Apron.mkIntVal(50))
    val v3 = v1.assignCopy(xVar, Apron.mkIntVal(100))
    val v4 = v2.joinCopy(v3)
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x(00) -50.0 >= 0;  -1x(00) +100.0 >= 0 }""".stripMargin)

    ApronMemoryManager.releaseMemory()
  }

  "Imposing constraints on a valuation" should "be correct" in {
    val v1 = emptyValuationOctagon(debug = false).createUninitializedVariable(xVar)
    val xApronVar = v1.apronVariableAnyScope(xVar)
    val v2 = {
      val gt = Apron.mkGt(xApronVar, Apron.mkIntVal(3)) // x>3 <=> x>=4
      val le = Apron.mkGe(Apron.mkIntVal(10), xApronVar)
      v1.imposeConstraint(Conjunction(Singleton(gt), Singleton(le)))
    }
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x(00) -4.0 >= 0;  -1x(00) +10.0 >= 0 }""".stripMargin, "Conjunction")
    val v3 = {
      val eq1 = Apron.mkEq(xApronVar, Apron.mkIntVal(100))
      val eq2 = Apron.mkEq(Apron.mkIntVal(50), xApronVar)
      v1.imposeConstraint(Disjunction(Singleton(eq1), Singleton(eq2)))
    }
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x(00) -50.0 >= 0;  -1x(00) +100.0 >= 0 }""".stripMargin, "Disjunction")

    ApronMemoryManager.releaseMemory()
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
    val scope1: Scope = Some(s1)
    val scope2: Scope = Some(s2)
    val scope3: Scope = Some(s3)
    val map: Map[BrboAst, Statement] = Map(s2.asInstanceOf[BrboAst] -> scope1.get)
    val scopeOperations = new ScopeOperations(map)
    val v1 = AbstractMachine.createEmptyValuation(new Octagon(), None, scopeOperations)
    val v2 = v1.createInitializedVariable(Variable(x, scope1))
    val v3 = v2.createInitializedVariable(Variable(y, scope1))
    StringCompare.ignoreWhitespaces(v3.indexOfVariableThisScope(Variable(x, scope1)).toString, """0""", "Look up x in Scope 1")

    val v4 = v3.createInitializedVariable(Variable(x, scope2))
    StringCompare.ignoreWhitespaces(v4.indexOfVariableThisScope(Variable(x, scope2)).toString, """2""", "Look up x in Scope 2")
    StringCompare.ignoreWhitespaces(v4.indexOfVariableThisScope(Variable(y, scope2)).toString, """-1""", "Look up y in Scope 2")
    StringCompare.ignoreWhitespaces(v4.indexOfVariableThisOrParentScope(Variable(y, scope1)).toString, """1""", "Look up y in Scope 1")

    val v5 = v4.createInitializedVariable(Variable(x, scope3))
    val v6 = v5.createInitializedVariable(Variable(y, scope3))
    StringCompare.ignoreWhitespaces(v6.indexOfVariableThisScope(Variable(x, scope3)).toString, """3""", "Look up x in Scope 3")
    StringCompare.ignoreWhitespaces(v6.indexOfVariableThisScope(Variable(x, scope1)).toString, """0""", "Look up x in Scope 1 (again)")

    ApronMemoryManager.releaseMemory()
  }

  "Inclusion check between two valuations" should "be correct" in {
    val v1 = emptyValuationOctagon(debug = false).createInitializedVariable(xVar)
    val xApronVar = v1.apronVariableAnyScope(xVar)
    val v2 = v1.imposeConstraint(Singleton(Apron.mkEq(xApronVar, Apron.mkIntVal(10))))
    val v3 = v1.imposeConstraint(Singleton(Apron.mkGe(xApronVar, Apron.mkIntVal(0))))
    StringCompare.compareLiteral(v2.include(v2).toString, "true")
    StringCompare.compareLiteral(v3.include(v2).toString, "true") // x>=0 include x=10
    StringCompare.compareLiteral(v2.include(v3).toString, "false")

    ApronMemoryManager.releaseMemory()
  }

  "Check the satisfaction of polynomial constraints for valuations" should "be correct" in {
    val v1Polka = emptyValuationStrictPolka(debug = false)
      .createUninitializedVariable(xVar).createUninitializedVariable(zVar).createUninitializedVariable(aVar)
    val xApronVarPolka = v1Polka.apronVariableAnyScope(xVar)
    val zApronVarPolka = v1Polka.apronVariableAnyScope(zVar)
    val aApronVarPolka = v1Polka.apronVariableAnyScope(aVar)

    val v2Polka = v1Polka.imposeConstraint(Singleton(Apron.mkEq(xApronVarPolka, zApronVarPolka)))
      .imposeConstraint(Singleton(Apron.mkGtZero(aApronVarPolka)))
      .imposeConstraint(Singleton(Apron.mkGtZero(xApronVarPolka)))
    val multiplication = Apron.mkMul(aApronVarPolka, zApronVarPolka)
    val constraint = Apron.mkLe(xApronVarPolka, multiplication)
    StringCompare.ignoreWhitespaces(v2Polka.toString,
      """Variables:
        |  Variable(x,None)
        |  Variable(z,None)
        |  Variable(a,None)
        |ApronState: {  -1x(00) +1z(01) = 0;  1a(02) -1 >= 0;  1x(00) -1 >= 0 }""".stripMargin)
    StringCompare.ignoreWhitespaces(v2Polka.satisfy(constraint).toString, "false", "Check with Apron")
    StringCompare.ignoreWhitespaces(v2Polka.satisfyWithZ3(constraint).toString, "true", "Check with Z3")

    ApronMemoryManager.releaseMemory()
  }

  "Check the satisfaction of constraints for valuations" should "be correct" in {
    // val xFloat = Identifier("x", BrboType.FLOAT)
    // val xVar = Variable(xFloat, None)
    val v1Octagon = emptyValuationOctagon(debug = false).createUninitializedVariable(xVar)
    val xApronVarOctagon = v1Octagon.apronVariableAnyScope(xVar)

    val v1Polka = emptyValuationStrictPolka(debug = false).createUninitializedVariable(xVar)
    val xApronVarPolka = v1Polka.apronVariableAnyScope(xVar)

    val xValue = 10

    val v2Octagon = v1Octagon.imposeConstraint(Singleton(Apron.mkEq(xApronVarOctagon, Apron.mkIntVal(xValue))))
    StringCompare.ignoreWhitespaces(v2Octagon.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x(00) -10.0 >= 0;  -1x(00) +10.0 >= 0 }""".stripMargin)

    val v2Polka = v1Polka.imposeConstraint(Singleton(Apron.mkEq(xApronVarPolka, Apron.mkIntVal(xValue))))
    StringCompare.ignoreWhitespaces(v2Polka.toString,
      """Variables:
        |  Variable(x,None)
        |ApronState: {  1x(00) -10 = 0 }""".stripMargin)

    StringCompare.compareLiteral(v2Octagon.satisfy(greaterThanOrEqualTo(x, Number(10))).toString, "true", "Satisfy BrboExpr: x>=10 (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(greaterThan(x, Number(100))).toString, "false", "Satisfy BrboExpr: x>100 (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Bool(b = true)).toString, "true", "Satisfy BrboExpr: true (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Bool(b = false)).toString, "false", "Satisfy BrboExpr: false (Octagon)")

    StringCompare.compareLiteral(v2Polka.satisfy(greaterThanOrEqualTo(x, Number(10))).toString, "true", "Satisfy BrboExpr: x>=10 (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(greaterThan(x, Number(100))).toString, "false", "Satisfy BrboExpr: x>100 (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Bool(b = true)).toString, "true", "Satisfy BrboExpr: true (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Bool(b = false)).toString, "false", "Satisfy BrboExpr: false (Polka)")

    val eqOctagon: Tcons0 = Apron.mkEq(xApronVarOctagon, Apron.mkIntVal(xValue))
    val gtOctagon = Apron.mkGt(xApronVarOctagon, Apron.mkIntVal(100))
    val ltOctagon = Apron.mkLt(xApronVarOctagon, Apron.mkIntVal(100))
    StringCompare.compareLiteral(v2Octagon.satisfy(eqOctagon).toString, "true", "Satisfy Tcons: Eq (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(gtOctagon).toString, "false", "Satisfy Tcons: Gt (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(ltOctagon).toString, "true", "Satisfy Tcons: Lt (Octagon)")

    val eqPolka: Tcons0 = Apron.mkEq(xApronVarPolka, Apron.mkIntVal(xValue))
    val gePolka = Apron.mkGe(xApronVarPolka, Apron.mkIntVal(100))
    val gtPolka = Apron.mkGt(xApronVarPolka, Apron.mkIntVal(9))
    val lePolka = Apron.mkLe(xApronVarPolka, Apron.mkIntVal(100))
    val ltPolka = Apron.mkLt(xApronVarPolka, Apron.mkIntVal(100))
    StringCompare.compareLiteral(v2Polka.satisfy(eqPolka).toString, "true", "Satisfy Tcons: Eq (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(gePolka).toString, "false", "Satisfy Tcons: Ge (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(gtPolka).toString, "true", "Satisfy Tcons: Gt (Polka)") // false under non-strict Polka
    StringCompare.compareLiteral(v2Polka.satisfy(lePolka).toString, "true", "Satisfy Tcons: Le (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(ltPolka).toString, "true", "Satisfy Tcons: Lt (Polka)") // false under non-strict Polka

    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(eqOctagon)).toString, "true", "Satisfy Singleton: Eq (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(gtOctagon)).toString, "false", "Satisfy Singleton: Gt (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(Apron.mkBoolVal(true))).toString, "true", "Satisfy Singleton: true (Octagon)")
    StringCompare.compareLiteral(v2Octagon.satisfy(Singleton(Apron.mkBoolVal(false))).toString, "false", "Satisfy Singleton: false (Octagon)")

    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(eqPolka)).toString, "true", "Satisfy Singleton: Eq (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(gtPolka)).toString, "true", "Satisfy Singleton: Gt (Polka)") // false under non-strict Polka
    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(Apron.mkBoolVal(true))).toString, "true", "Satisfy Singleton: true (Polka)")
    StringCompare.compareLiteral(v2Polka.satisfy(Singleton(Apron.mkBoolVal(false))).toString, "false", "Satisfy Singleton: false (Polka)")

    val conjunctionOctagon = {
      val ge = Apron.mkGe(xApronVarOctagon, Apron.mkIntVal(10))
      val lt = Apron.mkLt(xApronVarOctagon, Apron.mkDoubleVal(10.5))
      Conjunction(Singleton(ge), Singleton(lt))
    }
    StringCompare.compareLiteral(v2Octagon.satisfy(conjunctionOctagon).toString, "true", "Satisfy Conjunction (Octagon)")

    val conjunctionPolka = Conjunction(Singleton(eqPolka), Singleton(lePolka))
    StringCompare.compareLiteral(v2Polka.satisfy(conjunctionPolka).toString, "true", "Satisfy Conjunction (Polka)")

    val disjunctionOctagon = Disjunction(Singleton(gtOctagon), Singleton(ltOctagon))
    StringCompare.compareLiteral(v2Octagon.satisfy(disjunctionOctagon).toString, "true", "Satisfy Disjunction (Octagon)")

    val disjunctionPolka = Disjunction(Singleton(gePolka), Singleton(lePolka))
    StringCompare.compareLiteral(v2Polka.satisfy(disjunctionPolka).toString, "true", "Satisfy Disjunction (Polka)")

    ApronMemoryManager.releaseMemory()
    ApronMemoryManager.releaseMemory()
  }

  "Evaluating commands VariableDeclaration and Assignment" should "be correct" in {
    val v0Polka = declareInputsAAndB(emptyValuationStrictPolka(debug = false))
    StringCompare.ignoreWhitespaces(v0Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |ApronState: {  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin, "Initialize inputs (Polka)")

    val v0Octagon = declareInputsAAndB(emptyValuationOctagon(debug = false))
    StringCompare.ignoreWhitespaces(v0Octagon.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |ApronState: {  1a(00) -1.0 >= 0;  1a(00) +1b(01) -2.0 >= 0;  1b(01) -1.0 >= 0 }""".stripMargin, "Initialize inputs (Octagon)")

    val v1Polka = initializeGhostVariables(v0Polka)
    StringCompare.ignoreWhitespaces(v1Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  1C1(04) +1 = 0;  1S1(03) = 0;  1R1(02) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      "Initialize ghost variables (Polka)")

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
      AbstractMachine.evalCommand(v, Assignment(rStar, Addition(rStar, a)), None, debugLogger)
    }

    val v2Polka = rStarAddA(v1Polka)
    StringCompare.ignoreWhitespaces(v2Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) +1S1(03) = 0;  1C1(04) +1 = 0;  1R1(02) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      "When r* = 0, do r* = r* + a (Polka)")
    val v2Octagon = rStarAddA(v1Octagon)

    // r* = r* + b
    def rStarAddB(v: Valuation): Valuation = {
      AbstractMachine.evalCommand(v, Assignment(rStar, Addition(rStar, b)), None, debugLogger)
    }

    val v3Polka = rStarAddB(v2Polka)
    StringCompare.ignoreWhitespaces(v3Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) -1b(01) +1S1(03) = 0;  1C1(04) +1 = 0;  1R1(02) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      "When r* = a, do r* = r* + b (Polka)")
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
        |ApronState: {  -2a(00) +1S1(03) = 0;  1C1(04) +1 = 0;  1R1(02) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      "When r* = a, do r* = r* + a (Polka)")
    val v4Octagon = rStarAddA(v2Octagon)
    val rStarEqTwoA = {
      val rStarApron = v3Octagon.apronVariableAnyScope(rStarVar)
      val aApron = v3Octagon.apronVariableAnyScope(aVar)
      Singleton(Apron.mkEq(rStarApron, Apron.mkAdd(aApron, aApron)))
    }
    StringCompare.compareLiteral(v4Octagon.satisfy(rStarEqTwoA).toString, "false", "When r* = a, do r* = r* + a (Octagon)")

    // r = r + a; r = r + 5;
    def rAddAAndFive(v: Valuation): Valuation = {
      val v1 = AbstractMachine.evalCommand(v, Assignment(r, rStar), None, debugLogger)
      AbstractMachine.evalCommand(v1, Assignment(r, Addition(r, Number(5))), None, debugLogger)
    }

    val v5Polka = rAddAAndFive(v2Polka)
    StringCompare.ignoreWhitespaces(v5Polka.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) +1S1(03) = 0;  -1a(00) +1R1(02) -5 = 0;  1C1(04) +1 = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      "When r = a, do r = r + 5 (Polka)")
    val v5Octagon = rAddAAndFive(v2Octagon)
    val rEqAPlusFive = {
      val rApron = v3Octagon.apronVariableAnyScope(rVar)
      val aApron = v3Octagon.apronVariableAnyScope(aVar)
      Singleton(Apron.mkEq(rApron, Apron.mkAdd(aApron, Apron.mkIntVal(5))))
    }
    StringCompare.compareLiteral(v5Octagon.satisfy(rEqAPlusFive).toString, "true", "When r = a, do r = r + a (Octagon)")

    ApronMemoryManager.releaseMemory()
    ApronMemoryManager.releaseMemory()
  }

  "Evaluating ndInt() in assignment commands" should "be correct" in {
    val v0Polka = emptyValuationStrictPolka(debug = false)
    val v1Polka = v0Polka.createInitializedVariable(xVar)
    StringCompare.ignoreWhitespaces(v1Polka.toShortString, """ApronState: {  1x = 0 }""")
    val callNdInt = FunctionCallExpr(PreDefinedFunctions.NDINT, Nil, BrboType.INT)
    val v2Polka = evalCommand(v1Polka, Assignment(xVar.identifier, callNdInt), debug = false)
    StringCompare.ignoreWhitespaces(v2Polka.toShortString, """ApronState: <universal>""")

    ApronMemoryManager.releaseMemory()
  }

  "Evaluating commands Use and Reset" should "be correct" in {
    val reset: Reset = Reset(1)
    val use: Use = Use(Some(1), a)
    val v0 = initializeGhostVariables(declareInputsAAndB(emptyValuationStrictPolka(debug = false)))
    val v1 = {
      val debug = true
      val v1 = evalCommand(v0, reset, debug)
      val v2 = evalCommand(v1, use, debug)
      val v3 = evalCommand(v2, reset, debug)
      AbstractMachine.evalCommand(v3, use, None)
    }
    StringCompare.ignoreWhitespaces(v1.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) +1S1(03) = 0;  -1a(00) +1R1(02) = 0;  1C1(04) -1 = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      s"$reset; $use; $reset; $use")

    val v2 = {
      val debug = false
      val v1 = evalCommand(v0, reset, debug)
      val v2 = evalCommand(v1, use, debug)
      evalCommand(v2, use, debug)
    }
    StringCompare.ignoreWhitespaces(v2.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -2a(00) +1R1(02) = 0;  1C1(04) = 0;  1S1(03) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      s"$reset; $use; $use")

    val bothPossible = Equal(a, Number(5)) // Because a>0
    val onlyTrue = greaterThan(a, Number(-1)) // Because a>0
    val onlyFalse = lessThanOrEqualTo(a, Number(-1)) // Because a>0

    val resetBoth = Reset(1, bothPossible)
    val v3 = {
      val debug = false
      val v1 = evalCommand(v0, use, debug)
      evalCommand(v1, resetBoth, debug)
    }
    StringCompare.ignoreWhitespaces(v3.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) +1R1(02) +1S1(03) = 0;  -1C1(04) >= 0;  1C1(04) +1 >= 0;  1R1(02) +1C1(04) >= 0;  1b(01) -1 >= 0;  1a(00) -1R1(02) -1C1(04) -1 >= 0 }""".stripMargin,
      s"$use; $resetBoth")

    val resetTrue = Reset(1, onlyTrue)
    val v4 = {
      val debug = false
      val v1 = evalCommand(v0, use, debug)
      evalCommand(v1, resetTrue, debug)
    }
    StringCompare.ignoreWhitespaces(v4.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) +1S1(03) = 0;  1C1(04) = 0;  1R1(02) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      s"$use; $resetTrue")

    val resetFalse = Reset(1, onlyFalse)
    val v5 = {
      val debug = false
      val v1 = evalCommand(v0, use, debug)
      evalCommand(v1, resetFalse, debug)
    }
    StringCompare.ignoreWhitespaces(v5.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) +1R1(02) = 0;  1C1(04) +1 = 0;  1S1(03) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      s"$use; $resetFalse")

    val useBoth = Use(Some(1), a, bothPossible)
    val v6 = {
      val debug = false
      evalCommand(v0, useBoth, debug)
    }
    StringCompare.ignoreWhitespaces(v6.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  1C1(04) +1 = 0;  1S1(03) = 0;  1R1(02) >= 0;  1b(01) -1 >= 0;  1a(00) -1R1(02) >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      s"$useBoth")

    val useTrue = Use(Some(1), a, onlyTrue)
    val v7 = {
      val debug = false
      evalCommand(v0, useTrue, debug)
    }
    StringCompare.ignoreWhitespaces(v7.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  -1a(00) +1R1(02) = 0;  1C1(04) +1 = 0;  1S1(03) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin,
      s"$useTrue")

    val useFalse = Use(Some(1), a, onlyFalse)
    val v8 = {
      val debug = false
      evalCommand(v0, useFalse, debug)
    }
    StringCompare.ignoreWhitespaces(v8.toString,
      """Variables:
        |  Variable(a,None)
        |  Variable(b,None)
        |  Variable(R1,None)
        |  Variable(S1,None)
        |  Variable(C1,None)
        |ApronState: {  1C1(04) +1 = 0;  1S1(03) = 0;  1R1(02) = 0;  1b(01) -1 >= 0;  1a(00) -1 >= 0 }""".stripMargin, s"$useFalse")

    ApronMemoryManager.releaseMemory()
  }

  "Operations over lexical scopes" should "be correct" in {
    val scope1: Scope = Some(Block(List(Assignment(x, Number(1)))))
    val scope2: Scope = Some(Block(List(Assignment(x, Number(2)))))
    val scope3: Scope = Some(Block(List(Assignment(x, Number(3)))))
    val map: Map[BrboAst, Statement] = Map(scope1.get -> scope2.get, scope2.get -> scope3.get)
    val scopeOperations = new ScopeOperations(map)
    StringCompare.compareLiteral(scopeOperations.isSame(scope1, scope1).toString, "true", "isSame: true")
    StringCompare.compareLiteral(scopeOperations.isSame(scope1, scope2).toString, "false", "isSame: false")
    StringCompare.compareLiteral(scopeOperations.isSubScope(scope1, scope1).toString, "true", "isSubScope: true 1")
    StringCompare.compareLiteral(scopeOperations.isSubScope(scope1, scope2).toString, "true", "isSubScope: true 2")
    StringCompare.compareLiteral(scopeOperations.isSubScope(scope3, TOP_SCOPE).toString, "true", "isSubScope: true 3")
    StringCompare.compareLiteral(scopeOperations.isSubScope(scope2, scope1).toString, "false", "isSubScope: false")
    StringCompare.compareLiteral(scopeOperations.isStrictSubScope(scope1, scope2).toString, "true", "isStrictSubScope: true 1")
    StringCompare.compareLiteral(scopeOperations.isStrictSubScope(scope1, scope3).toString, "true", "isStrictSubScope: true 2")
    StringCompare.compareLiteral(scopeOperations.isStrictSubScope(scope1, scope1).toString, "false", "isStrictSubScope: false")
  }

  "Indices of variables in Apron states" should "be expected" in {
    val v1 = emptyValuationStrictPolka(debug = false)
    val v2 = v1.createUninitializedVariable(xVar).createInitializedVariable(yVar)
      .createUninitializedVariable(zVar).createInitializedVariable(aVar)
    StringCompare.ignoreWhitespaces(v2.toShortString, """ApronState: {  1a = 0;  1y = 0 }""")

    ApronMemoryManager.releaseMemory()
  }

  "Forgetting the values of variables" should "be correct" in {
    val v1 = emptyValuationStrictPolka(debug = false)
    val v2 = v1.createInitializedVariable(xVar).createInitializedVariable(yVar)
    StringCompare.ignoreWhitespaces(v2.toShortString, """ApronState: {  1y = 0;  1x = 0 }""")
    val v3 = v2.forgetVariable(xVar).forgetVariable(yVar)
    StringCompare.ignoreWhitespaces(v3.toShortString, """ApronState: <universal>""")

    ApronMemoryManager.releaseMemory()
  }

  // Initialize r, r*, r#
  private def initializeGhostVariables(v: Valuation): Valuation = {
    val v1 = AbstractMachine.evalCommand(v, VariableDeclaration(r, Number(0)), None)
    val v2 = AbstractMachine.evalCommand(v1, VariableDeclaration(rStar, Number(0)), None)
    AbstractMachine.evalCommand(v2, VariableDeclaration(rCounter, Number(-1)), None)
  }

  // Declare inputs: a, b
  private def declareInputsAAndB(v: Valuation): Valuation = {
    val v0 = v.createUninitializedVariable(aVar).createUninitializedVariable(bVar)
    val aApronVar = v0.apronVariableAnyScope(aVar)
    val bApronVar = v0.apronVariableAnyScope(bVar)
    v0.imposeConstraint(Conjunction(Singleton(Apron.mkGtZero(aApronVar)), Singleton(Apron.mkGtZero(bApronVar))))
  }

  private def evalCommand(v: Valuation, command: Command, debug: Boolean): Valuation = {
    if (debug) AbstractMachine.evalCommand(v, command, None, debugLogger)
    else AbstractMachine.evalCommand(v, command, None)
  }
}*/

object AbstractMachineUnitTest {
}
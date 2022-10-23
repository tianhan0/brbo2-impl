package brbo.backend.verifier

import brbo.TestCase
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType._
import brbo.common.Z3Solver
import brbo.common.ast.BrboExprUtils.{greaterThan, lessThanOrEqualTo}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicExecutionUnitTest extends AnyFlatSpec {
  /*"Symbolically executing a path" should "be correct" in {
    SymbolicExecutionUnitTest.tests.foreach({
      testCase =>
        val symbolicExecution = new SymbolicExecution(SymbolicExecutionUnitTest.program.mainFunction.parameters, debugMode = false)
        val solver = new Z3Solver
        val result = symbolicExecution.execute(testCase.input.asInstanceOf[Path].pathNodes, solver).finalState
        StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"${testCase.name} failed!")
    })
  }*/
}

object SymbolicExecutionUnitTest {
  val i: Identifier = Identifier("i", INT)
  val x: Identifier = Identifier("x", INT)
  val R: Identifier = Identifier("R", INT)
  val n: Identifier = Identifier("n", INT)
  val a: Identifier = Identifier("a", INT)
  val b: Identifier = Identifier("b", INT)
  val e: Identifier = Identifier("e", INT)

  val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n, a, b), Block(Nil), Set(2))
  val assumeFunction: BrboFunction = PreDefinedFunctions.Assume.internalRepresentation
  val ndBoolFunction: BrboFunction = PreDefinedFunctions.NdBool.internalRepresentation
  val ndIntFunction: BrboFunction = PreDefinedFunctions.NdInt.internalRepresentation
  val assertFunction: BrboFunction = PreDefinedFunctions.Assert.internalRepresentation
  val program: BrboProgram = BrboProgram("Test program", mainFunction, Nil, List(assumeFunction, ndBoolFunction, ndIntFunction))

  val assumeCond: BrboExpr = assumeFunction.parameters.head
  val assertCond: BrboExpr = assertFunction.parameters.head

  val tests: List[TestCase] = {
    val test01 = {
      // This path comes from UAutomizerVerifierUnitTest.scala
      Path(List(
        CFGNode((VariableDeclaration(i, Number(0))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((VariableDeclaration(R, Number(0))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((BeforeFunctionCall(assumeFunction, List(greaterThan(n, Number(0))))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Negation(Negation(assumeCond))), Some(assumeFunction), CFGNode.DONT_CARE_ID),
        CFGNode((FunctionExit()), Some(assumeFunction), CFGNode.DONT_CARE_ID),
        CFGNode((BeforeFunctionCall(ndBoolFunction, Nil)), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((BeforeFunctionCall(ndIntFunction, Nil)), Some(ndBoolFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Return(Some(FunctionCallExpr(PreDefinedFunctions.VerifierNondetInt.name, Nil, INT)))), Some(ndIntFunction), CFGNode.DONT_CARE_ID),
        CFGNode((VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))), Some(ndBoolFunction), CFGNode.DONT_CARE_ID),
        CFGNode((BeforeFunctionCall(assumeFunction, List(Or(Equal(x, Number(0)), Equal(x, Number(1)))))), Some(ndBoolFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Negation(Negation(assumeCond))), Some(assumeFunction), CFGNode.DONT_CARE_ID),
        CFGNode((FunctionExit()), Some(assumeFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Return(Some(x))), Some(ndBoolFunction), CFGNode.DONT_CARE_ID),
        CFGNode((LessThan(i, n)), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((VariableDeclaration(e, Number(0))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((LessThan(i, Number(1))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Assignment(e, a)), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Assignment(R, Addition(R, e))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Assignment(i, Addition(i, Number(1)))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Negation(LessThan(i, Number(1)))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((BeforeFunctionCall(assertFunction, List(lessThanOrEqualTo(R, a)))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Negation(assertCond)), Some(assertFunction), CFGNode.DONT_CARE_ID),
        CFGNode((LabeledCommand("ERROR", FunctionCallExpr(PreDefinedFunctions.VerifierError.name, Nil, VOID))), Some(assertFunction), CFGNode.DONT_CARE_ID),
        CFGNode((Return(None)), Some(assertFunction), CFGNode.DONT_CARE_ID)
      ))
    }

    val test02 = {
      val use = Use(Some(2), Number(1), greaterThan(n, a))
      val reset = Reset(2, greaterThan(n, b))
      Path(List(
        CFGNode((VariableDeclaration(use.resourceVariable, Number(0))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((VariableDeclaration(reset.starVariable, Number(0))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((VariableDeclaration(reset.counterVariable, Number(-1))), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((use), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((reset), Some(mainFunction), CFGNode.DONT_CARE_ID),
        CFGNode((use), Some(mainFunction), CFGNode.DONT_CARE_ID),
      ))
    }

    List(
      TestCase("Test Counterexample Path", test01,
        """Valuations:
          |  (R,(INT,Value((+ 0 a))))
          |  (a,(INT,Value(a)))
          |  (b,(INT,Value(b)))
          |  (e,(INT,Value(a)))
          |  (i,(INT,Value((+ 0 1))))
          |  (n,(INT,Value(n)))
          |Path condition: (let ((a!1 (not (not (not (< n 0)))))
          |      (a!2 (not (not (or (= v5 0) (= v5 1)))))
          |      (a!3 (not (or (< (+ 0 a) a) (= (+ 0 a) a)))))
          |  (and true a!1 a!2 (< 0 n) (< 0 1) (not (< (+ 0 1) 1)) a!3))
          |Return values:
          |  (ndBool,List(Value(v5)))
          |  (ndInt,List())""".stripMargin),
      TestCase("Test Uses and Resets", test02,
        """Valuations:
          |  (C2,(INT,Value((+ (- 1) 1))))
          |  (R2,(INT,Value((+ 0 1))))
          |  (S2,(INT,Value((ite (< 0 (+ 0 1)) (+ 0 1) 0))))
          |  (a,(INT,Value(a)))
          |  (b,(INT,Value(b)))
          |  (n,(INT,Value(n)))
          |Path condition: (and true (not (< n a)) (not (< n b)) (not (< n a)))
          |Return values:
          |""".stripMargin)
    )
  }
}
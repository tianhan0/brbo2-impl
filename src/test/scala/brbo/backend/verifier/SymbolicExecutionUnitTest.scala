package brbo.backend.verifier

import brbo.TestCase
import brbo.backend.verifier.cex.Path
import brbo.common.StringCompare
import brbo.common.TypeUtils.BrboType._
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicExecutionUnitTest extends AnyFlatSpec {
  SymbolicExecutionUnitTest.tests.foreach({
    testCase =>
      val symbolicExecution = new SymbolicExecution(testCase.input.asInstanceOf[Path], SymbolicExecutionUnitTest.program)
      assert(StringCompare.ignoreWhitespaces(symbolicExecution.result.toString, testCase.expectedOutput, s"${testCase.name} failed!"))
  })
}

object SymbolicExecutionUnitTest {
  val i: Identifier = Identifier("i", INT)
  val x: Identifier = Identifier("x", INT)
  val R: Identifier = Identifier("R", INT)
  val n: Identifier = Identifier("n", INT)
  val a: Identifier = Identifier("a", INT)
  val b: Identifier = Identifier("b", INT)
  val e: Identifier = Identifier("e", INT)

  val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n, a, b), Block(Nil))
  val assumeFunction: BrboFunction = PreDefinedBrboFunctions.assume
  val ndBoolFunction: BrboFunction = PreDefinedBrboFunctions.ndBool
  val ndIntFunction: BrboFunction = PreDefinedBrboFunctions.ndInt
  val assertFunction: BrboFunction = PreDefinedBrboFunctions.assert
  val program: BrboProgram = BrboProgram("Test program", mainFunction, None, None, List(assumeFunction, ndBoolFunction, ndIntFunction))

  val assumeCond: BrboExpr = assumeFunction.parameters.head
  val assertCond: BrboExpr = assertFunction.parameters.head

  val tests: List[TestCase] = {
    val test01 = {
      // This path comes from UAutomizerVerifierUnitTest.scala
      Path(List(
        CFGNode(Left(VariableDeclaration(i, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(VariableDeclaration(R, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(assumeFunction, List(GreaterThan(n, Number(0))))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(Negative(Negative(assumeCond))), assumeFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(FunctionExit()), assumeFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(ndBoolFunction, Nil)), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(ndIntFunction, Nil)), ndBoolFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(Return(Some(FunctionCallExpr(PreDefinedBrboFunctions.VERIFIER_NONDET_INT, Nil, INT)))), ndIntFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))), ndBoolFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(assumeFunction, List(Or(Equal(x, Number(0)), Equal(x, Number(1)))))), ndBoolFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(Negative(Negative(assumeCond))), assumeFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(FunctionExit()), assumeFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(Return(Some(x))), ndBoolFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(LessThan(i, n)), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(VariableDeclaration(e, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(LessThan(i, Number(1))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(Assignment(e, a)), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(Assignment(R, Addition(R, e))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(Assignment(i, Addition(i, Number(1)))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(Negative(LessThan(i, Number(1)))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(assertFunction, List(LessThanOrEqualTo(R, a)))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(Negative(assertCond)), assertFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(LabeledCommand("ERROR", FunctionCall(FunctionCallExpr("__VERIFIER_error", Nil, VOID)))), assertFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(Return(None)), assertFunction, CFGNode.DONT_CARE_ID)
      ))
    }
    List(
      TestCase("Test 01", test01,
        """Valuations:
          |  (R,Value((+ 0 a)))
          |  (a,Value(a))
          |  (b,Value(b))
          |  (e,Value(a))
          |  (i,Value((+ 0 1)))
          |  (n,Value(n))
          |Path condition: (let ((a!1 (not (not (or (= v3 0) (= v3 1))))))
          |  (and true
          |       (not (not (> n 0)))
          |       a!1
          |       (< 0 n)
          |       (< 0 1)
          |       (not (< (+ 0 1) 1))
          |       (not (<= (+ 0 a) a))))
          |Return values:
          |  (ndBool,List(Value(v3)))
          |  (ndInt,List())""".stripMargin)
    )
  }
}
package brbo.backend.verifier

import brbo.TestCase
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType._
import brbo.common.StringCompare
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicExecutionUnitTest extends AnyFlatSpec {
  "Symbolic execution a path" should "be correct" in {
    SymbolicExecutionUnitTest.tests.foreach({
      testCase =>
        val symbolicExecution = new SymbolicExecution(SymbolicExecutionUnitTest.program.mainFunction.parameters)
        val result = symbolicExecution.execute(testCase.input.asInstanceOf[Path].pathNodes)
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"${testCase.name} failed!"))
    })
  }
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
  val assumeFunction: BrboFunction = PreDefinedFunctions.assume
  val ndBoolFunction: BrboFunction = PreDefinedFunctions.ndBool
  val ndIntFunction: BrboFunction = PreDefinedFunctions.ndInt
  val assertFunction: BrboFunction = PreDefinedFunctions.assert
  val program: BrboProgram = BrboProgram("Test program", mainFunction, Set[Int](), None, None, List(assumeFunction, ndBoolFunction, ndIntFunction))

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
        CFGNode(Left(Return(Some(FunctionCallExpr(PreDefinedFunctions.VERIFIER_NONDET_INT, Nil, INT)))), ndIntFunction, CFGNode.DONT_CARE_ID),
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

    val test02 = {
      val use = Use(Some(2), Number(1))
      val reset = Reset(2)
      Path(List(
        CFGNode(Left(VariableDeclaration(use.resourceVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(VariableDeclaration(reset.sharpVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(VariableDeclaration(reset.counterVariable, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
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
          |  (ndInt,List())""".stripMargin),
      TestCase("Test Uses and Resets", test02,
        """Valuations:
          |  (C2,(INT,Value((+ 0 1))))
          |  (R2,(INT,Value((+ 0 1))))
          |  (S2,(INT,Value((ite (< 0 (+ 0 1)) (+ 0 1) 0))))
          |  (a,(INT,Value(a)))
          |  (b,(INT,Value(b)))
          |  (n,(INT,Value(n)))
          |Path condition: true
          |Return values:
          |""".stripMargin)
    )
  }
}
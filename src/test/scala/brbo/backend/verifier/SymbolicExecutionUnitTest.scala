package brbo.backend.verifier

import brbo.TestCase
import brbo.backend.verifier.cex.Path
import brbo.common.TypeUtils.BrboType.{BOOL, INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicExecutionUnitTest extends AnyFlatSpec {
  SymbolicExecutionUnitTest.tests.foreach({
    testCase =>
      val symbolicExecution = new SymbolicExecution(testCase.input.asInstanceOf[Path], SymbolicExecutionUnitTest.program)
      println(symbolicExecution.result)
  })
}

object SymbolicExecutionUnitTest {
  val i: Identifier = Identifier("i", INT)
  val x: Identifier = Identifier("x", INT)
  val R: Identifier = Identifier("R", INT)
  val n: Identifier = Identifier("n", INT)
  val cond: Identifier = Identifier("cond", BOOL)

  val mainFunction: BrboFunction = BrboFunction("main", VOID, List(n), Block(Nil))
  val assumeFunction: BrboFunction = BrboFunction("assume", VOID, List(cond), Block(Nil))
  val ndBoolFunction: BrboFunction = BrboFunction("ndBool", VOID, Nil, Block(Nil))
  val ndIntFunction: BrboFunction = BrboFunction("ndInt", VOID, Nil, Block(Nil))
  val program: BrboProgram = BrboProgram("Test program", mainFunction, List(assumeFunction, ndBoolFunction, ndIntFunction))

  val tests: List[TestCase] = {
    val test01 = {
      Path(List(
        CFGNode(Left(VariableDeclaration(i, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(VariableDeclaration(R, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(assumeFunction, List(GreaterThan(n, Number(0))))), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(Negative(Negative(cond))), assumeFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(ndBoolFunction, Nil)), mainFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(ndIntFunction, Nil)), ndBoolFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(Return(Some(FunctionCallExpr(PreDefinedBrboFunctions.VERIFIER_NONDET_INT, Nil, INT)))), ndIntFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))), ndBoolFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Left(CallFunction(assumeFunction, List(Or(Equal(x, Number(0)), Equal(x, Number(1)))))), ndBoolFunction, CFGNode.DONT_CARE_ID),
        CFGNode(Right(Negative(Negative(cond))), assumeFunction, CFGNode.DONT_CARE_ID),
      ))
    }
    List(
      TestCase("Test 01", test01, "")
    )
  }
}
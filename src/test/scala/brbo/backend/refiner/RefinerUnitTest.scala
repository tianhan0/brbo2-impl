package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.refiner.RefinerUnitTest._
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{INT, VOID}
import brbo.common.CommandLineArguments
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.scalatest.flatspec.AnyFlatSpec

class RefinerUnitTest extends AnyFlatSpec {
  val refiner = new Refiner(program, CommandLineArguments.DEFAULT_ARGUMENTS)

  "Refining a program" should "succeed" in {
    testCases.foreach({
      testCase =>
        val (newProgram, refinement) = refiner.refine(program, Some(testCase.input.asInstanceOf[Path]), boundExpression, Set())
        println(refinement)
        println(newProgram)
    })
  }
}

object RefinerUnitTest {
  private val i: Identifier = Identifier("i", INT)
  private val n: Identifier = Identifier("n", INT)
  private val a: Identifier = Identifier("a", INT)
  private val b: Identifier = Identifier("b", INT)
  private val e: Identifier = Identifier("e", INT)
  private val boundExpression: BrboExpr = Addition(a, Multiplication(b, ITEExpr(GreaterThan(Number(0), Subtraction(n, Number(1))), Number(0), Subtraction(n, Number(1)))))
  private val declaration = VariableDeclaration(i, Number(0))
  private val assignWithA = Assignment(e, a)
  private val assignWithB = Assignment(e, b)
  private val increment = Assignment(i, Addition(i, Number(1)))
  private val loopCondition = LessThan(i, n)
  private val iteCondition = Equal(i, Number(0))
  private val use = Use(Some(1), e)
  private val reset = Reset(1)

  private val mainFunction: BrboFunction = {
    val loop = {
      val ite = ITE(iteCondition, assignWithA, assignWithB)
      val loopBody = Block(List(ite, reset, use, increment))
      Loop(loopCondition, loopBody)
    }
    BrboFunction("main", VOID, List(n, a, b), Block(List(declaration, loop)), Set(1))
  }
  val program: BrboProgram = BrboProgram("Test program", mainFunction, None, None, Nil)

  val testCases: List[TestCase] = {
    val path = Path(List(
      CFGNode(Left(mainFunction.ghostVariableInitializations(0)), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(mainFunction.ghostVariableInitializations(1)), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(mainFunction.ghostVariableInitializations(2)), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(declaration), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Right(loopCondition), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Right(iteCondition), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(assignWithA), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(increment), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Right(loopCondition), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Right(Negative(iteCondition)), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(assignWithB), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID),
      CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
    ))

    List(
      TestCase("Refine Test 01", path, """""")
    )
  }
}
package brbo.common.ast

import apron.{Octagon, Polka}
import brbo.TestCase
import brbo.backend.verifier.modelchecker.AbstractMachine
import brbo.backend.verifier.modelchecker.AbstractMachine.Variable
import brbo.common.BrboType.INT
import brbo.common.ast.BrboExprUnitTest._
import brbo.common.{BrboType, MyLogger, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class BrboExprUnitTest extends AnyFlatSpec {
  "Pretty-printing BrboExpr to C expression" should "be correct" in {
    BrboExprUnitTest.prettyPrintToCUnitTest.foreach({
      testCase =>
        val expr = testCase.input.asInstanceOf[BrboExpr]
        StringCompare.compareLiteral(expr.prettyPrintToC(), testCase.expectedOutput, s"${testCase.name} failed!")
        BrboExprUtils.visit(expr) // Ensure every expression is handled by this pattern matching
    })
  }

  private val valuation = {
    val manager = new Octagon() // new Polka(false)
    val logger = Some(MyLogger.createLogger(classOf[BrboExprUnitTest], debugMode = false))
    val valuation = AbstractMachine.createEmptyValuation(manager, None, logger)
    valuation.createUninitializedVariable(Variable(x, None))
      .createUninitializedVariable(Variable(y, None))
      .createUninitializedVariable(Variable(z, None))
  }
  "Translating expressions to Apron" should "be correct" in {
    BrboExprUnitTest.toApronTest.foreach({
      testCase =>
        val (apronValue, newValuation) = BrboExprUtils.toApron(testCase.input.asInstanceOf[BrboExpr], valuation, None)
        StringCompare.ignoreWhitespaces(s"$apronValue\n$newValuation", testCase.expectedOutput, s"${testCase.name} failed!")
    })
  }
}

object BrboExprUnitTest {
  val prettyPrintToCUnitTest: List[TestCase] =
    List[TestCase](
      TestCase("Identifier", Identifier("z", BrboType.INT), "z"),
      TestCase("Boolean", Bool(b = true), "true"),
      TestCase("Number", Number(23), "23"),
      TestCase("StringLiteral", StringLiteral("helloworld"), "helloworld"),
      TestCase("Addition", Addition(Number(2), Number(3)), "(2 + 3)"),
      TestCase("Subtraction", Subtraction(Number(2), Number(3)), "(2 - 3)"),
      TestCase("Multiplication", Multiplication(Number(2), Number(3)), "(2 * 3)"),
      TestCase("Division", Division(Number(2), Number(3)), "(2 / 3)"),
      TestCase("Negation", Negation(Bool(b = true)), "!(true)"),
      TestCase("LessThan", LessThan(Number(2), Number(3)), "(2 < 3)"),
      TestCase("LessThanOrEqualTo", LessThanOrEqualTo(Number(2), Number(3)), "(2 <= 3)"),
      TestCase("GreaterThan", GreaterThan(Number(2), Number(3)), "(2 > 3)"),
      TestCase("GreaterThanOrEqualTo", GreaterThanOrEqualTo(Number(2), Number(3)), "(2 >= 3)"),
      TestCase("Equal", Equal(Number(2), Number(3)), "(2 == 3)"),
      TestCase("NotEqual", NotEqual(Number(2), Number(3)), "(2 != 3)"),
      TestCase("And", And(Bool(b = true), Bool(b = false)), "(true && false)"),
      TestCase("Or", Or(Bool(b = true), Bool(b = false)), "(true || false)"),
      TestCase("FunctionCallExpr", FunctionCallExpr("f", List(Identifier("a", INT), Identifier("b", INT), Identifier("c", INT)), BrboType.INT), "f(a, b, c)"),
      TestCase("ITEExpr", ITEExpr(Bool(b = true), Number(0), Number(1)), "true ? 0 : 1"),
      TestCase("Imply", Imply(Bool(b = true), Bool(b = false)), "(!true || false)"),
    )

  private val x = Identifier("x", BrboType.INT)
  private val y = Identifier("y", BrboType.INT)
  private val z = Identifier("z", BrboType.BOOL)
  val toApronTest: List[TestCase] = {
    List[TestCase](
      TestCase("IdentifierInt", x,
        """ApronExpr(x0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("IdentifierBool", z,
        """Singleton(x2 <> 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Boolean", Bool(b = true),
        """Singleton(1.0 - 1.0 = 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Number", Number(23),
        """ApronExpr(23.0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Addition", Addition(x, Number(3)),
        """ApronExpr(x0 + 3.0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Addition2", Addition(x, y),
        """ApronExpr(x0 + x1)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Subtraction", Subtraction(x, Number(3)),
        """ApronExpr(x0 - 3.0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Multiplication", Multiplication(x, Number(3)),
        """ApronExpr(x0 * 3.0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Division", Division(x, Number(3)),
        """ApronExpr(x0 / 3.0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Negation", Negation(Bool(b = true)),
        """Singleton(1.0 - 1.0 <> 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("LessThan", LessThan(x, Number(3)),
        """Singleton(3.0 - x0 > 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("LessThanOrEqualTo", LessThanOrEqualTo(x, Number(3)),
        """Singleton(3.0 - x0 >= 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("GreaterThan", GreaterThan(x, Number(3)),
        """Singleton(x0 - 3.0 > 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("GreaterThanOrEqualTo", GreaterThanOrEqualTo(x, Number(3)),
        """Singleton(x0 - 3.0 >= 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Equal", Equal(x, Number(3)),
        """Singleton(x0 - 3.0 = 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("NotEqual", NotEqual(x, Number(3)),
        """Singleton(x0 - 3.0 <> 0)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("And", And(Bool(b = true), Bool(b = false)),
        """Conjunction(Singleton(1.0 - 1.0 = 0),Singleton(1.0 = 0))
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("Or", Or(Bool(b = true), Bool(b = false)),
        """Disjunction(Singleton(1.0 - 1.0 = 0),Singleton(1.0 = 0))
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      TestCase("ITEExpr", ITEExpr(Bool(b = true), Bool(b = true), Bool(b = false)),
        """Conjunction(Disjunction(Singleton(1.0 - 1.0 <> 0),Singleton(1.0 - 1.0 = 0)),Disjunction(Singleton(1.0 - 1.0 = 0),Singleton(1.0 = 0)))
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
      /*TestCase("ITEExpr2", ITEExpr(Bool(b = true), Number(2), Number(3)),
        """ApronExpr(x3)
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |  Variable(v!3,None)
          |ApronState: {  1x3 -2.0 >= 0;  -1x3 +2.0 >= 0 }""".stripMargin),*/
      TestCase("Imply", Imply(Bool(b = true), Bool(b = false)),
        """Disjunction(Singleton(1.0 - 1.0 <> 0),Singleton(1.0 = 0))
          |Variables:
          |  Variable(x,None)
          |  Variable(y,None)
          |  Variable(z,None)
          |ApronState: <universal>""".stripMargin),
    )
  }
}

package brbo.backend.verifier.modelchecker

import apron._
import brbo.TestCase
import brbo.backend.verifier.modelchecker.ApronUnitTest._
import brbo.common.ast.Identifier
import brbo.common.string.StringCompare
import brbo.common.{BrboType, Z3Solver}
import org.scalatest.flatspec.AnyFlatSpec

class ApronUnitTest extends AnyFlatSpec {
  "Translating expressions to Z3 (treating doubles as integers)" should "be correct" in {
    expressionTestsToInt.foreach({
      test =>
        val solver = new Z3Solver
        val actual = Apron.expressionToZ3(test.input.asInstanceOf[Texpr0Node], solver, variables)
        StringCompare.ignoreWhitespaces(actual.toString, test.expectedOutput, s"Test `${test.name}` failed!")
    })
  }

  "Translating constraints to Z3 (treating doubles as integers)" should "be correct" in {
    constraintTestToInt.foreach({
      test =>
        val solver = new Z3Solver
        val actual = Apron.constraintToZ3(test.input.asInstanceOf[Tcons0], solver, variables)
        StringCompare.ignoreWhitespaces(actual.toString, test.expectedOutput, s"Test `${test.name}` failed!")
    })
  }
}

object ApronUnitTest {
  private val x = Apron.mkVar(index = 0)
  private val y = Apron.mkVar(index = 1)
  private val number1 = Apron.mkDoubleVal(1)
  private val number2 = Apron.mkDoubleVal(2)
  private val number3 = Apron.mkDoubleVal(-3)
  private val variables = List(
    Identifier("x", BrboType.INT),
    Identifier("y", BrboType.INT)
  )
  private val testAdd = Apron.mkAdd(x, Apron.mkAdd(y, number1))
  private val testSub = Apron.mkSub(x, Apron.mkSub(y, number2))
  private val testMul = Apron.mkMul(x, Apron.mkMul(y, number3))
  private val testDiv = Apron.mkDiv(x, Apron.mkDiv(y, number1))
  private val testNegative1 = Apron.mkNegative(x)
  private val testNegative2 = Apron.mkNegative(number2)

  val expressionTests: List[TestCase] = {
    List(
      TestCase("TestVarX", x, "x"),
      TestCase("TestVarY", y, "y"),
      TestCase("TestVal1", number1, "1"),
      TestCase("TestVal2", number2, "2"),
      TestCase("TestVal3", number3, "(- 3)"),
      TestCase("TestAdd", testAdd, """(fp.add roundNearestTiesToEven
                                     |        x
                                     |        (fp.add roundNearestTiesToEven y (fp #b0 #b01111111111 #x0000000000000)))""".stripMargin),
      TestCase("TestSub", testSub, """(fp.sub roundNearestTiesToEven
                                     |        x
                                     |        (fp.sub roundNearestTiesToEven y (fp #b0 #b10000000000 #x0000000000000)))""".stripMargin),
      TestCase("TestMul", testMul, """(fp.mul roundNearestTiesToEven
                                     |        x
                                     |        (fp.mul roundNearestTiesToEven y (fp #b1 #b10000000000 #x8000000000000)))""".stripMargin),
      TestCase("TestDiv", testDiv, """(fp.div roundNearestTiesToEven
                                     |        x
                                     |        (fp.div roundNearestTiesToEven y (fp #b0 #b01111111111 #x0000000000000)))""".stripMargin),
      TestCase("TestNegative1", testNegative1, "(fp.sub roundNearestTiesToEven (_ +zero 11 53) x)"),
      TestCase("TestNegative2", testNegative2, """(fp.sub roundNearestTiesToEven
                                                 |        (_ +zero 11 53)
                                                 |        (fp #b0 #b10000000000 #x0000000000000))""".stripMargin),
    )
  }

  val expressionTestsToInt: List[TestCase] = {
    List(
      TestCase("TestVarX", x, "x"),
      TestCase("TestVarY", y, "y"),
      TestCase("TestVal1", number1, "1"),
      TestCase("TestVal2", number2, "2"),
      TestCase("TestVal3", number3, "-3"),
      TestCase("TestAdd", testAdd, """(+ x y 1)""".stripMargin),
      TestCase("TestSub", testSub, """(- x (- y 2))""".stripMargin),
      TestCase("TestMul", testMul, """(* x y (- 3))""".stripMargin),
      TestCase("TestDiv", testDiv, """(div x (div y 1))""".stripMargin),
      TestCase("TestNegative1", testNegative1, "(- 0 x)"),
      TestCase("TestNegative2", testNegative2, "(- 0 2)"),
    )
  }

  private val gez = Apron.mkGeZero(testAdd)
  private val gtz = Apron.mkGtZero(testSub)
  private val eqz = Apron.mkEqZero(testMul)
  private val nez = Apron.mkNeZero(testDiv)
  private val ge = Apron.mkGe(x, number2)
  private val gt = Apron.mkGt(x, number2)
  private val le = Apron.mkLe(x, number2)
  private val lt = Apron.mkLt(x, number2)

  val constraintTest: List[TestCase] = {
    List(
      TestCase("TestGeZero", gez, """(fp.geq (fp.add roundNearestTiesToEven
                               |                x
                               |                (fp.add roundNearestTiesToEven
                               |                        y
                               |                        (fp #b0 #b01111111111 #x0000000000000)))
                               |        (_ +zero 11 53))""".stripMargin),
      TestCase("TestGtZero", gtz, """(fp.gt (fp.sub roundNearestTiesToEven
                                    |               x
                                    |               (fp.sub roundNearestTiesToEven
                                    |                       y
                                    |                       (fp #b0 #b10000000000 #x0000000000000)))
                                    |       (_ +zero 11 53))""".stripMargin),
      TestCase("TestEqZero", eqz, """(= (fp.mul roundNearestTiesToEven
                                    |           x
                                    |           (fp.mul roundNearestTiesToEven
                                    |                   y
                                    |                   (fp #b1 #b10000000000 #x8000000000000)))
                                    |   (_ +zero 11 53))""".stripMargin),
      TestCase("TestNeZero", nez, """(not (= (fp.div roundNearestTiesToEven
                               |                x
                               |                (fp.div roundNearestTiesToEven
                               |                        y
                               |                        (fp #b0 #b01111111111 #x0000000000000)))
                               |        (_ +zero 11 53)))""".stripMargin),
      TestCase("TestGe", ge, """(fp.geq (fp.sub roundNearestTiesToEven x (fp #b0 #b10000000000 #x0000000000000))
                               |        (_ +zero 11 53))""".stripMargin),
      TestCase("TestGt", gt, """(fp.gt (fp.sub roundNearestTiesToEven x (fp #b0 #b10000000000 #x0000000000000))
                               |       (_ +zero 11 53))""".stripMargin),
      TestCase("TestLe", le, """(fp.geq (fp.sub roundNearestTiesToEven (fp #b0 #b10000000000 #x0000000000000) x)
                               |        (_ +zero 11 53))""".stripMargin),
      TestCase("TestLt", lt, """(fp.gt (fp.sub roundNearestTiesToEven (fp #b0 #b10000000000 #x0000000000000) x)
                               |       (_ +zero 11 53))""".stripMargin),
    )
  }

  val constraintTestToInt: List[TestCase] = {
    List(
      TestCase("TestGeZero", gez, """(>= (+ x y 1) 0)""".stripMargin),
      TestCase("TestGtZero", gtz, """(> (- x (- y 2)) 0)""".stripMargin),
      TestCase("TestEqZero", eqz, """(= (* x y (- 3)) 0)""".stripMargin),
      TestCase("TestNeZero", nez, """(not (= (div x (div y 1)) 0))""".stripMargin),
      TestCase("TestGe", ge, """(>= (- x 2) 0)""".stripMargin),
      TestCase("TestGt", gt, """(> (- x 2) 0)""".stripMargin),
      TestCase("TestLe", le, """(>= (- 2 x) 0)""".stripMargin),
      TestCase("TestLt", lt, """(> (- 2 x) 0)""".stripMargin),
    )
  }
}

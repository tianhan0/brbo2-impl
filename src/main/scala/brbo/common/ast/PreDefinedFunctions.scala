package brbo.common.ast

import brbo.common.BrboType.{BOOL, INT, VOID}
import brbo.common.ast.BrboExprUtils.{greaterThan, lessThanOrEqualTo}
import brbo.frontend.TargetProgram

object PreDefinedFunctions {
  val VERIFIER_ERROR: String = "__VERIFIER_error"
  val ABORT: String = "abort"
  val VERIFIER_NONDET_INT: String = "__VERIFIER_nondet_int"

  /*val __VERIFIER_error: BrboFunction = BrboFunction(VERIFIER_ERROR, VOID, Nil, Block(Nil))
  val abort: BrboFunction = BrboFunction(ABORT, VOID, Nil, Block(Nil))
  val __VERIFIER_nondet_int: BrboFunction = BrboFunction(VERIFIER_NONDET_INT, INT, Nil, Block(Nil))

  val UNDEFINED_FUNCTIONS: Map[String, BrboFunction] = Map(
    VERIFIER_ERROR -> __VERIFIER_error,
    ABORT -> abort,
    VERIFIER_NONDET_INT -> __VERIFIER_nondet_int
  )*/

  val UNDEFINED_FUNCTIONS_MACRO: String =
    s"""extern void $VERIFIER_ERROR() __attribute__((noreturn));
       |extern void $ABORT(void);
       |extern int $VERIFIER_NONDET_INT ();""".stripMargin

  val SYMBOLS_MACRO: String = {
    val predefinedVariables = TargetProgram.PREDEFINED_VARIABLES.map({ case (name, value) => s"#define $name $value" }).toList.sorted.mkString("\n")
    s"""#define true 1
       |#define false 0
       |#define boolean int
       |$predefinedVariables
       |""".stripMargin
  }

  /*val __VERIFIER_assert: BrboFunction = {
    val cond = Identifier("cond", BOOL)
    val body = {
      val ite = {
        val condition = Negative(cond)
        val thenStatement = LabeledCommand("ERROR", FunctionCall(None, FunctionCallExpr("__VERIFIER_error", Nil, VOID)))
        val elseStatement = Skip()
        ITE(condition, thenStatement, elseStatement)
      }
      Block(List(ite, Return(None)))
    }
    BrboFunction("__VERIFIER_assert", VOID, List(cond), body)
  }*/

  val ASSERT: String = "assert"
  val ASSUME: String = "assume"
  val NDINT: String = "ndInt"
  val NDBOOL: String = "ndBool"
  val NDINT2: String = "ndInt2"
  val NDINT3: String = "ndInt3"
  val UNINITIALIZED: String = "uninitialized"
  val BOUND_ASSERTION: String = "boundAssertion"

  val assertFunction: BrboFunction = {
    val cond = Identifier("cond", BOOL)
    val body = {
      val ite = {
        val condition = Negation(cond)
        val thenStatement = LabeledCommand("ERROR", FunctionCall(FunctionCallExpr("__VERIFIER_error", Nil, VOID)))
        val elseStatement = Skip()
        ITE(condition, thenStatement, elseStatement)
      }
      Block(List(ite, Return(None)))
    }
    BrboFunction(ASSERT, VOID, List(cond), body, Set())
  }

  val assumeFunction: BrboFunction = {
    val cond = Identifier("cond", BOOL)
    val body = {
      val ite = {
        val condition = Negation(cond)
        val thenStatement = FunctionCall(FunctionCallExpr("abort", Nil, VOID))
        val elseStatement = Skip()
        ITE(condition, thenStatement, elseStatement)
      }
      Block(List(ite))
    }
    BrboFunction(ASSUME, VOID, List(cond), body, Set())
  }

  val ndIntFunction: BrboFunction = {
    val body = {
      val returnCommand = Return(Some(FunctionCallExpr("__VERIFIER_nondet_int", Nil, INT)))
      Block(List(returnCommand))
    }
    BrboFunction(NDINT, INT, Nil, body, Set())
  }

  val ndBoolFunction: BrboFunction = {
    val body = {
      val x = Identifier("x", INT)
      val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
      val ite = ITE(greaterThan(x, Number(0)), Return(Some(Bool(b = true))), Return(Some(Bool(b = false))))
      Block(List(variableDeclaration, ite))
    }
    BrboFunction(NDBOOL, BOOL, Nil, body, Set())
  }

  val ndInt2Function: BrboFunction = {
    val lower = Identifier("lower", INT)
    val upper = Identifier("upper", INT)
    val body = {
      val x = Identifier("x", INT)
      val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
      val assume = createAssume(And(lessThanOrEqualTo(lower, x), lessThanOrEqualTo(x, upper)))
      val returnCommand = Return(Some(x))
      Block(List(variableDeclaration, assume, returnCommand))
    }
    BrboFunction(NDINT2, INT, List(lower, upper), body, Set())
  }

  val ndInt3Function: BrboFunction = {
    val x = Identifier("x", INT)
    val lower = Identifier("lower", INT)
    val upper = Identifier("upper", INT)
    val body = {
      val assume = createAssume(And(lessThanOrEqualTo(lower, x), lessThanOrEqualTo(x, upper)))
      Block(List(assume))
    }
    BrboFunction(NDINT3, VOID, List(lower, x, upper), body, Set())
  }

  val uninitializedFunction: BrboFunction = {
    BrboFunction(UNINITIALIZED, INT, List(), Block(List()), Set())
  }

  val boundAssertFunction: BrboFunction = {
    BrboFunction(BOUND_ASSERTION, VOID, List(Identifier("assertion", BOOL)), Block(Nil), Set())
  }

  val allFunctions = Map(ASSERT -> assertFunction, ASSUME -> assumeFunction, NDINT -> ndIntFunction,
    NDBOOL -> ndBoolFunction, NDINT2 -> ndInt2Function, NDINT3 -> ndInt3Function,
    UNINITIALIZED -> uninitializedFunction, BOUND_ASSERTION -> boundAssertFunction)
  val allFunctionsList = List(assertFunction, assumeFunction, ndIntFunction, ndBoolFunction, ndInt2Function, ndInt3Function, boundAssertFunction)

  def createAssert(expression: BrboExpr): FunctionCall = FunctionCall(FunctionCallExpr(ASSERT, List(expression), BOOL))

  def createAssume(expression: BrboExpr): FunctionCall = FunctionCall(FunctionCallExpr(ASSUME, List(expression), BOOL))
}

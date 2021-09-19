package brbo.common.ast

import brbo.common.TypeUtils.BrboType.{BOOL, INT, VOID}

object PreDefinedBrboFunctions {
  val VERIFIER_ERROR: String = "__VERIFIER_error"
  val ABORT: String = "abort"
  val VERIFIER_NONDET_INT: String = "__VERIFIER_nondet_int"

  val __VERIFIER_error: BrboFunction = BrboFunction(VERIFIER_ERROR, VOID, Nil, Block(Nil))
  val abort: BrboFunction = BrboFunction(ABORT, VOID, Nil, Block(Nil))
  val __VERIFIER_nondet_int: BrboFunction = BrboFunction(VERIFIER_NONDET_INT, INT, Nil, Block(Nil))

  val UNDEFINED_FUNCTIONS: Map[String, BrboFunction] = Map(
    VERIFIER_ERROR -> __VERIFIER_error,
    ABORT -> abort,
    VERIFIER_NONDET_INT -> __VERIFIER_nondet_int
  )

  val UNDEFINED_FUNCTIONS_MACRO: String =
    s"""extern void $VERIFIER_ERROR() __attribute__((noreturn));
       |extern void $ABORT(void);
       |extern int $VERIFIER_NONDET_INT ();""".stripMargin

  val SYMBOLS_MACRO: String =
    """#define true 1
      |#define false 0
      |#define boolean int
      |#define MAX 8
      |""".stripMargin

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

  val assert: BrboFunction = {
    val cond = Identifier("cond", BOOL)
    val body = {
      val ite = {
        val condition = Negative(cond)
        val thenStatement = LabeledCommand("ERROR", FunctionCall(FunctionCallExpr("__VERIFIER_error", Nil, VOID)))
        val elseStatement = Skip()
        ITE(condition, thenStatement, elseStatement)
      }
      Block(List(ite, Return(None)))
    }
    BrboFunction("assert", VOID, List(cond), body)
  }

  val assume: BrboFunction = {
    val cond = Identifier("cond", BOOL)
    val body = {
      val ite = {
        val condition = Negative(cond)
        val thenStatement = FunctionCall(FunctionCallExpr("abort", Nil, VOID))
        val elseStatement = Skip()
        ITE(condition, thenStatement, elseStatement)
      }
      Block(List(ite))
    }
    BrboFunction("assume", VOID, List(cond), body)
  }

  val ndInt: BrboFunction = {
    val body = {
      val returnCommand = Return(Some(FunctionCallExpr("__VERIFIER_nondet_int", Nil, INT)))
      Block(List(returnCommand))
    }
    BrboFunction("ndInt", INT, Nil, body)
  }

  val ndBool: BrboFunction = {
    val body = {
      val x = Identifier("x", INT)
      val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
      val assume = createAssume(Or(Equal(x, Number(0)), Equal(x, Number(1))))
      val returnCommand = Return(Some(x))
      Block(List(variableDeclaration, assume, returnCommand))
    }
    BrboFunction("ndBool", INT, Nil, body)
  }

  val ndInt2: BrboFunction = {
    val lower = Identifier("lower", INT)
    val upper = Identifier("upper", INT)
    val body = {
      val x = Identifier("x", INT)
      val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
      val assume = createAssume(And(LessThanOrEqualTo(lower, x), LessThanOrEqualTo(x, upper)))
      val returnCommand = Return(Some(x))
      Block(List(variableDeclaration, assume, returnCommand))
    }
    BrboFunction("ndInt2", INT, List(lower, upper), body)
  }

  val allFunctions = List(assert, assume, ndInt, ndBool, ndInt2)

  def createAssert(expression: BrboExpr): FunctionCall = FunctionCall(FunctionCallExpr("assert", List(expression), BOOL))

  def createAssume(expression: BrboExpr): FunctionCall = FunctionCall(FunctionCallExpr("assume", List(expression), BOOL))
}

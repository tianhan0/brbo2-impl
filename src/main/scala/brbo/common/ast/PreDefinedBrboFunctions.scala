package brbo.common.ast

import brbo.common.TypeUtils.BrboType.{BOOL, INT, VOID}

object PreDefinedBrboFunctions {
  val __VERIFIER_assert: BrboFunction = {
    val cond = Identifier("cond", BOOL)
    val body = {
      val ite = {
        val condition = Negative(cond)
        val thenStatement = LabeledCommand("ERROR", FunctionCall(None, FunctionCallExpr("__VERIFIER_error", Nil, VOID)))
        val elseStatement = Skip()
        ITE(condition, thenStatement, elseStatement)
      }
      Block(List(ite, ReturnVoid()))
    }
    BrboFunction("__VERIFIER_assert", VOID, List(cond), body)
  }

  val assert: BrboFunction = {
    val cond = Identifier("cond", BOOL)
    val body = {
      val ite = {
        val condition = Negative(cond)
        val thenStatement = LabeledCommand("ERROR", FunctionCall(None, FunctionCallExpr("__VERIFIER_error", Nil, VOID)))
        val elseStatement = Skip()
        ITE(condition, thenStatement, elseStatement)
      }
      Block(List(ite, ReturnVoid()))
    }
    BrboFunction("assert", VOID, List(cond), body)
  }

  val ndInt: BrboFunction = {
    val body = {
      val returnExpr = ReturnExpr(FunctionCallExpr("__VERIFIER_nondet_int", Nil, INT))
      Block(List(returnExpr))
    }
    BrboFunction("ndInt", INT, Nil, body)
  }

  val ndBool: BrboFunction = {
    val body = {
      val x = Identifier("x", INT)
      val variableDeclaration = VariableDeclaration("x", INT, FunctionCallExpr("ndInt", Nil, INT))
      val assume = Assume(Or(Equal(x, Number(0)), Equal(x, Number(1))))
      val returnExpr = ReturnExpr(x)
      Block(List(variableDeclaration, assume, returnExpr))
    }
    BrboFunction("ndBool", INT, Nil, body)
  }

  val ndInt2: BrboFunction = {
    val lower = Identifier("lower", INT)
    val upper = Identifier("upper", INT)
    val body = {
      val x = Identifier("x", INT)
      val variableDeclaration = VariableDeclaration("x", INT, FunctionCallExpr("ndInt", Nil, INT))
      val assume = Assume(And(LessThanOrEqualTo(lower, x), LessThanOrEqualTo(x, upper)))
      val returnExpr = ReturnExpr(x)
      Block(List(variableDeclaration, assume, returnExpr))
    }
    BrboFunction("ndInt2", INT, List(lower, upper), body)
  }

  val allFunctions = List(__VERIFIER_assert, assert, ndInt, ndBool, ndInt2)
}

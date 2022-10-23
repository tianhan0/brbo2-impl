package brbo.common.ast

import brbo.common.BrboType.{BOOL, INT, VOID}
import brbo.common.ast.BrboExprUtils.{greaterThan, lessThanOrEqualTo}
import brbo.frontend.TargetProgram

object PreDefinedFunctions {
  val ATOMIC_FUNCTIONS_C_DECLARATION: String = List(VerifierError, VerifierNondetInt, Abort).map(f => f.cRepresentation).mkString("\n")
  val SYMBOLS_MACRO: String = {
    val predefinedVariables = TargetProgram.PREDEFINED_VARIABLES.map({ case (name, value) => s"#define $name $value" }).toList.sorted.mkString("\n")
    s"""#define true 1
       |#define false 0
       |#define boolean int
       |$predefinedVariables
       |""".stripMargin
  }

  def createAssert(expression: BrboExpr): FunctionCallExpr = FunctionCallExpr(Assert.internalRepresentation.identifier, List(expression), BOOL)

  def createAssume(expression: BrboExpr): FunctionCallExpr = FunctionCallExpr(Assume.internalRepresentation.identifier, List(expression), BOOL)

  class NotExist extends Exception

  abstract class SpecialFunction(val name: String) {
    val javaFunctionName: String = name

    def cRepresentation: String = internalRepresentation.printToC(0)

    def internalRepresentation: BrboFunction
  }

  object VerifierError extends SpecialFunction("__VERIFIER_error") {
    override def cRepresentation: String = "extern void $VERIFIER_ERROR() __attribute__((noreturn));"

    override def internalRepresentation: BrboFunction = throw new NotExist
  }

  object VerifierNondetInt extends SpecialFunction("__VERIFIER_nondet_int") {
    override def cRepresentation: String = "extern void $ABORT(void);"

    override def internalRepresentation: BrboFunction = throw new NotExist
  }

  object Abort extends SpecialFunction("abort") {
    override def cRepresentation: String = "extern void $ABORT(void);"

    override def internalRepresentation: BrboFunction = throw new NotExist
  }

  object Assert extends SpecialFunction("assert") {
    override def internalRepresentation: BrboFunction = {
      val cond = Identifier("cond", BOOL)
      val body = {
        val ite = {
          val condition = Negation(cond)
          val thenStatement = LabeledCommand("ERROR", FunctionCallExpr("__VERIFIER_error", Nil, VOID))
          val elseStatement = Skip()
          ITE(condition, thenStatement, elseStatement)
        }
        Block(List(ite, Return(None)))
      }
      BrboFunction(name, VOID, List(cond), body, Set())
    }
  }

  object Assume extends SpecialFunction("assume") {
    override def internalRepresentation: BrboFunction = {
      val cond = Identifier("cond", BOOL)
      val body = {
        val ite = {
          val condition = Negation(cond)
          val thenStatement = FunctionCallExpr("abort", Nil, VOID)
          val elseStatement = Skip()
          ITE(condition, thenStatement, elseStatement)
        }
        Block(List(ite))
      }
      BrboFunction(name, VOID, List(cond), body, Set())
    }
  }

  object NdInt extends SpecialFunction("ndInt") {
    override def internalRepresentation: BrboFunction = {
      val body = {
        val returnCommand = Return(Some(FunctionCallExpr("__VERIFIER_nondet_int", Nil, INT)))
        Block(List(returnCommand))
      }
      BrboFunction(name, INT, Nil, body, Set())
    }
  }

  object NdInt2 extends SpecialFunction("ndInt2") {
    override def internalRepresentation: BrboFunction = {
      val lower = Identifier("lower", INT)
      val upper = Identifier("upper", INT)
      val body = {
        val x = Identifier("x", INT)
        val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
        val assume = createAssume(And(lessThanOrEqualTo(lower, x), lessThanOrEqualTo(x, upper)))
        val returnCommand = Return(Some(x))
        Block(List(variableDeclaration, assume, returnCommand))
      }
      BrboFunction(name, INT, List(lower, upper), body, Set())
    }
  }

  object NdInt3 extends SpecialFunction("ndInt3") {
    override def internalRepresentation: BrboFunction = {
      val x = Identifier("x", INT)
      val lower = Identifier("lower", INT)
      val upper = Identifier("upper", INT)
      val body = {
        val assume = createAssume(And(lessThanOrEqualTo(lower, x), lessThanOrEqualTo(x, upper)))
        Block(List(assume))
      }
      BrboFunction(name, VOID, List(lower, x, upper), body, Set())
    }
  }

  object NdBool extends SpecialFunction("ndBool") {
    override def internalRepresentation: BrboFunction = {
      val body = {
        val x = Identifier("x", INT)
        val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
        val ite = ITE(greaterThan(x, Number(0)), Return(Some(Bool(b = true))), Return(Some(Bool(b = false))))
        Block(List(variableDeclaration, ite))
      }
      BrboFunction(name, BOOL, Nil, body, Set())
    }
  }

  object BoundAssertion extends SpecialFunction("boundAssertion") {
    override def internalRepresentation: BrboFunction = throw new NotExist
  }

  object Uninitialize extends SpecialFunction("uninitialized") {
    override def internalRepresentation: BrboFunction = throw new NotExist
  }

  val SpecialFunctions: List[SpecialFunction] = List(
    VerifierError, VerifierNondetInt, Abort, Assert, Assume,
    NdInt, NdInt2, NdInt3, NdBool, BoundAssertion, Uninitialize
  )

  val SpecialFunctionInternalRepresentations: List[BrboFunction] = {
    SpecialFunctions.foldLeft(Nil: List[BrboFunction])({
      case (soFar, f) =>
        try {
          f.internalRepresentation :: soFar
        }
        catch {
          case _: NotExist => soFar
        }
    })
  }
}

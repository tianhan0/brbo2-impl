package brbo.common

import brbo.common.BrboType._
import brbo.common.ast.BrboExprUtils.{greaterThan, lessThanOrEqualTo}
import brbo.common.ast.PrintStyle.CStyle
import brbo.common.ast._

object PreDefinedFunctions {
  val ATOMIC_FUNCTIONS_C_DECLARATION: String =
    List(VerifierError, VerifierNondetInt, Abort, ArrayRead, ArrayLength)
      .map(f => f.cStringRepresentation).mkString("\n")
  val SYMBOLS_MACRO: String = {
    val predefinedVariables = PredefinedVariables.variables.map({ case (name, value) => s"#define $name $value" }).toList.sorted.mkString("\n")
    s"""#define true 1
       |#define false 0
       |#define boolean int
       |$predefinedVariables
       |""".stripMargin
  }

  def callAssert(expression: BrboExpr): FunctionCallExpr =
    FunctionCallExpr(Assert.cRepresentation.identifier, List(expression), BOOL)

  def callAssume(expression: BrboExpr): FunctionCallExpr =
    FunctionCallExpr(Assume.cRepresentation.identifier, List(expression), BOOL)

  class RepresentationNotExist extends Exception

  abstract class PreDefinedFunction(val name: String) {
    def cStringRepresentation: String = cRepresentation.print(indent = 0, style = CStyle)

    def cRepresentation: BrboFunction

    def returnType: BrboType.T
  }

  object VerifierError extends PreDefinedFunction("__VERIFIER_error") {
    override def cStringRepresentation: String = s"extern void $name() __attribute__((noreturn));"

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object VerifierNondetInt extends PreDefinedFunction("__VERIFIER_nondet_int") {
    override def cStringRepresentation: String = s"extern int $name();"

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.INT
  }

  object Abort extends PreDefinedFunction("abort") {
    override def cStringRepresentation: String = s"extern void $name(void);"

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object Assert extends PreDefinedFunction("assert") {
    override def cRepresentation: BrboFunction = {
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
      BrboFunction(name, VOID, List(cond), body, Set(), useResource = false)
    }

    def returnType: BrboType.T = cRepresentation.returnType
  }

  object Assume extends PreDefinedFunction("assume") {
    override def cRepresentation: BrboFunction = {
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
      BrboFunction(name, VOID, List(cond), body, Set(), useResource = false)
    }

    def returnType: BrboType.T = cRepresentation.returnType
  }

  object NdInt extends PreDefinedFunction("ndInt") {
    override def cRepresentation: BrboFunction = {
      val body = {
        val returnCommand = Return(Some(FunctionCallExpr("__VERIFIER_nondet_int", Nil, INT)))
        Block(List(returnCommand))
      }
      BrboFunction(name, INT, Nil, body, Set(), useResource = false)
    }

    def returnType: BrboType.T = cRepresentation.returnType
  }

  object NdInt2 extends PreDefinedFunction("ndInt2") {
    override def cRepresentation: BrboFunction = {
      val lower = Identifier("lower", INT)
      val upper = Identifier("upper", INT)
      val body = {
        val x = Identifier("x", INT)
        val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
        val assume = callAssume(And(lessThanOrEqualTo(lower, x), lessThanOrEqualTo(x, upper)))
        val returnCommand = Return(Some(x))
        Block(List(variableDeclaration, assume, returnCommand))
      }
      BrboFunction(name, INT, List(lower, upper), body, Set(), useResource = false)
    }

    def returnType: BrboType.T = cRepresentation.returnType
  }

  object NdInt3 extends PreDefinedFunction("ndInt3") {
    override def cRepresentation: BrboFunction = {
      val x = Identifier("x", INT)
      val lower = Identifier("lower", INT)
      val upper = Identifier("upper", INT)
      val body = {
        val assume = callAssume(And(lessThanOrEqualTo(lower, x), lessThanOrEqualTo(x, upper)))
        Block(List(assume))
      }
      BrboFunction(name, VOID, List(lower, x, upper), body, Set(), useResource = false)
    }

    def returnType: BrboType.T = cRepresentation.returnType
  }

  object NdBool extends PreDefinedFunction("ndBool") {
    override def cRepresentation: BrboFunction = {
      val body = {
        val x = Identifier("x", INT)
        val variableDeclaration = VariableDeclaration(x, FunctionCallExpr("ndInt", Nil, INT))
        val ite = ITE(greaterThan(x, Number(0)), Return(Some(Bool(b = true))), Return(Some(Bool(b = false))))
        Block(List(variableDeclaration, ite))
      }
      BrboFunction(name, BOOL, Nil, body, Set(), useResource = false)
    }

    def returnType: BrboType.T = cRepresentation.returnType
  }

  object NdBool2 extends PreDefinedFunction("ndBool2") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.BOOL
  }

  object BoundAssertion extends PreDefinedFunction("boundAssertion") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object UpperBound extends PreDefinedFunction("upperBound") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object Uninitialize extends PreDefinedFunction("uninitialized") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object Use extends PreDefinedFunction("use") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object Reset extends PreDefinedFunction("reset") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object ArrayRead extends PreDefinedFunction("arrayRead") {
    override def cStringRepresentation: String = s"extern int $name(int array, int index);"

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = throw new Exception
  }

  object ArrayLength extends PreDefinedFunction("arrayLength") {
    override def cStringRepresentation: String = s"extern int $name(int array);"

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.INT
  }

  object ArraySum extends PreDefinedFunction("arraySum") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.INT
  }

  object MostPreciseBound extends PreDefinedFunction("mostPreciseBound") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object LessPreciseBound extends PreDefinedFunction("lessPreciseBound") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  object ResetPlaceHolder extends PreDefinedFunction("resetPlaceHolder") {
    override def cStringRepresentation: String = throw new RepresentationNotExist

    override def cRepresentation: BrboFunction = throw new RepresentationNotExist

    def returnType: BrboType.T = BrboType.VOID
  }

  val functions: List[PreDefinedFunction] = List(
    VerifierError, VerifierNondetInt, Abort, // C functions whose semantics are built into verifiers
    Assert, Assume,
    NdInt, NdInt2, NdInt3, NdBool, NdBool2,
    BoundAssertion, UpperBound,
    Uninitialize,
    Use, Reset,
    ArrayRead, ArrayLength, ArraySum,
    MostPreciseBound, LessPreciseBound,
    ResetPlaceHolder
  )

  val functionCRepresentations: List[BrboFunction] = {
    functions.foldLeft(Nil: List[BrboFunction])({
      case (soFar, f) =>
        try {
          f.cRepresentation :: soFar
        }
        catch {
          case _: RepresentationNotExist => soFar
        }
    })
  }
}

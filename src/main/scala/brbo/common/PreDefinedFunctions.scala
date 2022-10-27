package brbo.common

import brbo.common.BrboType._
import brbo.common.ast.BrboExprUtils.{greaterThan, lessThanOrEqualTo}
import brbo.common.ast._
import brbo.frontend.TargetProgram

object PreDefinedFunctions {
  val ATOMIC_FUNCTIONS_C_DECLARATION: String =
    List(VerifierError, VerifierNondetInt, Abort, ArrayRead, ArrayLength)
      .map(f => f.cRepresentation).mkString("\n")
  val SYMBOLS_MACRO: String = {
    val predefinedVariables = TargetProgram.PREDEFINED_VARIABLES.map({ case (name, value) => s"#define $name $value" }).toList.sorted.mkString("\n")
    s"""#define true 1
       |#define false 0
       |#define boolean int
       |$predefinedVariables
       |""".stripMargin
  }

  def createAssert(expression: BrboExpr): FunctionCallExpr =
    FunctionCallExpr(Assert.internalRepresentation.identifier, List(expression), BOOL)

  def createAssume(expression: BrboExpr): FunctionCallExpr =
    FunctionCallExpr(Assume.internalRepresentation.identifier, List(expression), BOOL)

  class RepresentationNotExist extends Exception

  abstract class PreDefinedFunction(val name: String) {
    val javaFunctionName: String = name

    def cRepresentation: String = internalRepresentation.printToC(0)

    def internalRepresentation: BrboFunction
  }

  object VerifierError extends PreDefinedFunction("__VERIFIER_error") {
    override def cRepresentation: String = s"extern void $name() __attribute__((noreturn));"

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object VerifierNondetInt extends PreDefinedFunction("__VERIFIER_nondet_int") {
    override def cRepresentation: String = s"extern int $name();"

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object Abort extends PreDefinedFunction("abort") {
    override def cRepresentation: String = s"extern void $name(void);"

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object Assert extends PreDefinedFunction("assert") {
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

  object Assume extends PreDefinedFunction("assume") {
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

  object NdInt extends PreDefinedFunction("ndInt") {
    override def internalRepresentation: BrboFunction = {
      val body = {
        val returnCommand = Return(Some(FunctionCallExpr("__VERIFIER_nondet_int", Nil, INT)))
        Block(List(returnCommand))
      }
      BrboFunction(name, INT, Nil, body, Set())
    }
  }

  object NdInt2 extends PreDefinedFunction("ndInt2") {
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

  object NdInt3 extends PreDefinedFunction("ndInt3") {
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

  object NdBool extends PreDefinedFunction("ndBool") {
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

  object BoundAssertion extends PreDefinedFunction("boundAssertion") {
    override def cRepresentation: String = throw new RepresentationNotExist

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object UpperBound extends PreDefinedFunction("upperBound") {
    override def cRepresentation: String = throw new RepresentationNotExist

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object Uninitialize extends PreDefinedFunction("uninitialized") {
    override def cRepresentation: String = throw new RepresentationNotExist

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object Use extends PreDefinedFunction("use") {
    override def cRepresentation: String = throw new RepresentationNotExist

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object Reset extends PreDefinedFunction("reset") {
    override def cRepresentation: String = throw new RepresentationNotExist

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object ArrayRead extends PreDefinedFunction("arrayRead") {
    override def cRepresentation: String = s"extern int $name(int array, int index);"

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object ArrayLength extends PreDefinedFunction("arrayLength") {
    override def cRepresentation: String = s"extern int $name(int array);"

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  object ArraySum extends PreDefinedFunction("arraySum") {
    override def cRepresentation: String = throw new RepresentationNotExist

    override def internalRepresentation: BrboFunction = throw new RepresentationNotExist
  }

  val functions: List[PreDefinedFunction] = List(
    VerifierError, VerifierNondetInt, Abort, // C functions whose semantics are built into verifiers
    Assert, Assume,
    NdInt, NdInt2, NdInt3, NdBool,
    BoundAssertion, UpperBound,
    Uninitialize,
    Use, Reset,
    ArrayRead, ArrayLength, ArraySum
  )

  val functionInternalRepresentations: List[BrboFunction] = {
    functions.foldLeft(Nil: List[BrboFunction])({
      case (soFar, f) =>
        try {
          f.internalRepresentation :: soFar
        }
        catch {
          case _: RepresentationNotExist => soFar
        }
    })
  }
}

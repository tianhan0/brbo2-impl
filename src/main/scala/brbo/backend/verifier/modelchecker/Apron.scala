package brbo.backend.verifier.modelchecker

import apron._
import brbo.common.Z3Solver
import brbo.common.ast.Identifier
import com.microsoft.z3.AST
import org.apache.logging.log4j.LogManager

object Apron {
  private val logger = LogManager.getLogger(Apron.getClass.getName)
  val BOOLEAN_POSITIVE = 1
  val BOOLEAN_NEGATIVE = 0

  def mkAdd(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_ADD, left, right)

  def mkSub(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_SUB, left, right)

  def mkMul(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_MUL, left, right)

  def mkDiv(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_DIV, left, right)

  def mkVar(index: Int): Texpr0DimNode = new Texpr0DimNode(index)

  def mkIntVal(value: Int): Texpr0Node = {
    // Texpr0Node.fromLinexpr0(new Linexpr0(new Array[Linterm0](0), new DoubleScalar(value)))
    mkCst(value)
  }

  def makeDoubleVal(value: Double): Texpr0Node = {
    // Texpr0Node.fromLinexpr0(new Linexpr0(new Array[Linterm0](0), new DoubleScalar(value)))
    mkCst(value)
  }

  def mkBoolVal(value: Boolean): Texpr0Node = {
    // Texpr0Node.fromLinexpr0(new Linexpr0(new Array[Linterm0](0), new DoubleScalar(if (value) BOOLEAN_POSITIVE else BOOLEAN_NEGATIVE)))
    mkCst(if (value) BOOLEAN_POSITIVE else BOOLEAN_NEGATIVE)
  }

  private def mkCst(value: Double): Texpr0Node = {
    val node = new Texpr0CstNode()
    node.cst = new DoubleScalar(value)
    node
  }

  def mkNegative(node: Texpr0Node): Texpr0Node = new Texpr0UnNode(Texpr0UnNode.OP_NEG, node)

  // Texpr0Node and Texpr0Intern can be converted back and forth
  def mkEq(expression: Texpr0Node): Tcons0 =
    new Tcons0(Tcons0.EQ, new Texpr0Intern(expression))

  def mkGe(expression: Texpr0Node): Tcons0 =
    new Tcons0(Tcons0.SUPEQ, new Texpr0Intern(expression))

  def mkGt(expression: Texpr0Node): Tcons0 =
    new Tcons0(Tcons0.SUP, new Texpr0Intern(expression))

  def mkNe(expression: Texpr0Node): Tcons0 =
    new Tcons0(Tcons0.DISEQ, new Texpr0Intern(expression))

  def constraintToZ3(constraint: Tcons0, solver: Z3Solver, variables: List[Identifier]): AST = {
    val node = expressionToZ3(constraint.toTexpr0Node, solver, variables)
    constraint.kind match {
      case Tcons0.EQ => solver.mkEq(node, solver.mkDoubleVal(0))
      case Tcons0.SUPEQ => solver.mkFPGe(node, solver.mkDoubleVal(0))
      case Tcons0.SUP => solver.mkFPGt(node, solver.mkDoubleVal(0))
      case Tcons0.EQMOD => throw new Exception
      case Tcons0.DISEQ => solver.mkNe(node, solver.mkDoubleVal(0))
    }
  }

  def expressionToZ3(node: Texpr0Node, solver: Z3Solver, variables: List[Identifier]): AST = {
    node match {
      case node: Texpr0BinNode =>
        val left = node.lArg
        val right = node.rArg
        node.op match {
          case Texpr0BinNode.OP_ADD =>
            solver.mkFPAdd(expressionToZ3(left, solver, variables), expressionToZ3(right, solver, variables))
          case Texpr0BinNode.OP_SUB =>
            solver.mkFPSub(expressionToZ3(left, solver, variables), expressionToZ3(right, solver, variables))
          case Texpr0BinNode.OP_MUL =>
            solver.mkFPMul(expressionToZ3(left, solver, variables), expressionToZ3(right, solver, variables))
          case Texpr0BinNode.OP_DIV =>
            solver.mkFPDiv(expressionToZ3(left, solver, variables), expressionToZ3(right, solver, variables))
          case _ => throw new Exception
        }
      case node: Texpr0CstNode =>
        val lower: Double = {
          val array = Array(0.0)
          node.cst.inf().toDouble(array, 0)
          array.head
        }
        assert(!java.lang.Double.isInfinite(lower))
        val upper: Double = {
          val array = Array(0.0)
          node.cst.sup().toDouble(array, 0)
          array.head
        }
        assert(!java.lang.Double.isInfinite(upper))
        solver.mkDoubleVal((lower + upper) / 2)
      case node: Texpr0DimNode =>
        val variable = variables(node.dim)
        variable.typ match {
          case brbo.common.BrboType.INT => solver.mkDoubleVar(variable.identifier)
          case brbo.common.BrboType.BOOL => solver.mkDoubleVar(variable.identifier) // Apron only accepts numerals
          case _ => throw new Exception
        }
      case node: Texpr0UnNode =>
        node.op match {
          case Texpr0UnNode.OP_NEG => solver.mkFPSub(solver.mkDoubleVal(0), expressionToZ3(node.getArgument, solver, variables))
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}

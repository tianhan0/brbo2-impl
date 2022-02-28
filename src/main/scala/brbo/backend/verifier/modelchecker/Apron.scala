package brbo.backend.verifier.modelchecker

import apron._
import brbo.common.Z3Solver
import brbo.common.ast.Identifier
import com.microsoft.z3.AST
import gmp.Mpz
import org.apache.logging.log4j.LogManager

object Apron {
  private val logger = LogManager.getLogger(Apron.getClass.getName)
  val BOOLEAN_NEGATIVE = 0
  val BOOLEAN_POSITIVE = 1

  private val negativeInfinity = {
    val d = new DoubleScalar(0)
    d.setInfty(-1)
    mkCst(d)
  }

  private val positiveInfinity = {
    val d = new DoubleScalar(0)
    d.setInfty(1)
    mkCst(d)
  }

  case class ApronVariable(index: Int, constraint: Option[Constraint])

  // Represent the result of evaluating a BrboExpr into an Apron-compatible representation
  abstract class ApronRepr

  // Represent an integer-typed value
  case class ApronExpr(node: Texpr0Node) extends ApronRepr

  // Represent a boolean-typed value
  abstract class Constraint extends ApronRepr {
    def negate(): Constraint
  }

  case class Singleton(constraint: Tcons0) extends Constraint {
    override def negate(): Constraint = Singleton(Apron.mkNegation(constraint))
  }

  case class Conjunction(left: Constraint, right: Constraint) extends Constraint {
    override def negate(): Constraint = Disjunction(left.negate(), right.negate())
  }

  case class Disjunction(left: Constraint, right: Constraint) extends Constraint {
    override def negate(): Constraint = Conjunction(left.negate(), right.negate())
  }

  def imposeConstraint(apronState: Abstract0, constraint: Constraint): Set[Abstract0] = {
    constraint match {
      case Conjunction(left, right) =>
        imposeConstraint(apronState, left).flatMap({ newState => imposeConstraint(newState, right) })
      case Disjunction(left, right) =>
        imposeConstraint(apronState, left) ++ imposeConstraint(apronState, right)
      case Singleton(constraint) => Set(apronState.meetCopy(apronState.getCreationManager, constraint))
      case _ => throw new Exception
    }
  }

  def createConjunction(list: Iterable[Constraint]): Constraint = {
    assert(list.nonEmpty)
    list.foldLeft(list.head)({ (acc, c) => Conjunction(acc, c) })
  }

  def createDisjunction(list: Iterable[Constraint]): Constraint = {
    assert(list.nonEmpty)
    list.foldLeft(list.head)({ (acc, c) => Disjunction(acc, c) })
  }

  def mkAdd(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_ADD, left, right)

  def mkSub(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_SUB, left, right)

  def mkMul(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_MUL, left, right)

  def mkDiv(left: Texpr0Node, right: Texpr0Node): Texpr0Node = new Texpr0BinNode(Texpr0BinNode.OP_DIV, left, right)

  def mkVar(index: Int): Texpr0DimNode = new Texpr0DimNode(index)

  def mkIntVal(value: Int): Texpr0Node = {
    // Texpr0Node.fromLinexpr0(new Linexpr0(new Array[Linterm0](0), new DoubleScalar(value)))
    /*val node = new Texpr0CstNode()
    val s = new MpfrScalar()
    s.set(value)
    node.cst = s
    node*/
    mkCst(value)
  }

  def mkDoubleVal(value: Double): Texpr0Node = mkCst(value)

  def mkPositiveInfinity: Texpr0Node = positiveInfinity

  def mkNegativeInfinity: Texpr0Node = negativeInfinity

  /*def mkNoConstraint(index: Int): Constraint = {
    val variable = mkVar(index)
    val le = mkGeZero(mkSub(mkPositiveInfinity, variable))
    val ge = mkGeZero(mkSub(variable, mkNegativeInfinity))
    Conjunction(Singleton(le), Singleton(ge))
  }*/

  def mkBoolVal(value: Boolean): Tcons0 = {
    // Bool-typed value b is translated into constraint 1=1 or 1=0
    if (value) mkEq(mkIntVal(1), mkIntVal(1)) // 1=1
    else mkEqZero(mkIntVal(1)) // 1=0
  }

  private def mkCst(value: DoubleScalar): Texpr0Node = {
    val node = new Texpr0CstNode()
    node.cst = value
    node
  }

  private def mkCst(value: Double): Texpr0Node = mkCst(new DoubleScalar(value))

  def mkNegative(node: Texpr0Node): Texpr0Node = new Texpr0UnNode(Texpr0UnNode.OP_NEG, node)

  // Texpr0Node and Texpr0Intern can be converted back and forth
  def mkEqZero(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.EQ, new Texpr0Intern(expression))

  def mkGeZero(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.SUPEQ, new Texpr0Intern(expression))

  def mkGtZero(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.SUP, new Texpr0Intern(expression))

  def mkNeZero(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.DISEQ, new Texpr0Intern(expression))

  def mkEq(left: Texpr0Node, right: Texpr0Node): Tcons0 = mkEqZero(mkSub(left, right))

  def mkNe(left: Texpr0Node, right: Texpr0Node): Tcons0 = mkNeZero(mkSub(left, right))

  def mkGe(left: Texpr0Node, right: Texpr0Node): Tcons0 = mkGeZero(mkSub(left, right))

  @deprecated // NOTE: Do not use this in the Polka domain (due to the unexpectedly wrong behavior). Use mkGe instead.
  def mkGt(left: Texpr0Node, right: Texpr0Node): Tcons0 = mkGtZero(mkSub(left, right))

  def mkLe(left: Texpr0Node, right: Texpr0Node): Tcons0 = mkGeZero(mkSub(right, left))

  @deprecated // NOTE: Do not use this in the Polka domain (due to the unexpectedly wrong behavior). Use mkLe instead.
  def mkLt(left: Texpr0Node, right: Texpr0Node): Tcons0 = mkGtZero(mkSub(right, left))

  def mkNegation(constraint: Tcons0): Tcons0 = {
    constraint.kind match {
      case Tcons0.EQ => Apron.mkNeZero(constraint.toTexpr0Node)
      case Tcons0.SUPEQ => Apron.mkGeZero(Apron.mkNegative(constraint.toTexpr0Node))
      case Tcons0.SUP => Apron.mkGtZero(Apron.mkNegative(constraint.toTexpr0Node))
      case Tcons0.EQMOD => throw new Exception
      case Tcons0.DISEQ => Apron.mkEqZero(constraint.toTexpr0Node)
    }
  }

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
          node.cst.inf().toDouble(array, Texpr0Node.RTYPE_DOUBLE)
          array.head
        }
        assert(!java.lang.Double.isInfinite(lower))
        val upper: Double = {
          val array = Array(0.0)
          node.cst.sup().toDouble(array, Texpr0Node.RTYPE_DOUBLE)
          array.head
        }
        assert(!java.lang.Double.isInfinite(upper))
        solver.mkDoubleVal((lower + upper) / 2)
      case node: Texpr0DimNode =>
        // TODO: This resolution from dimensions to variable names is wrong, because there may exist
        //  temporary variables that cannot be found in `variables`.
        val variable = variables(node.dim)
        variable.typ match {
          case brbo.common.BrboType.INT => solver.mkDoubleVar(variable.name)
          case brbo.common.BrboType.BOOL => solver.mkDoubleVar(variable.name) // Apron only accepts numerals
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

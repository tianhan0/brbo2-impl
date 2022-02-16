package brbo.backend.verifier.modelchecker

import apron._
import brbo.backend.verifier.modelchecker.AbstractDomainName._
import brbo.common.BrboType.BrboType
import brbo.common.ast.{BrboFunction, Statement}

class AbstractMachine {
  val manager = new Octagon // new Polka(false)
  val state = new Abstract0(manager, 3, 0)
  val a = new Texpr0DimNode(0)
  val rStar = new Texpr0DimNode(1)
  val b = new Texpr0DimNode(2)
  val initialConstraint = new Tcons0(Tcons0.SUPEQ, new Texpr0Intern(new Texpr0BinNode(Texpr0BinNode.OP_SUB, a, rStar)))
  val ten = Texpr0Node.fromLinexpr0(new Linexpr0(new Array[Linterm0](0), new DoubleScalar(10)))
  val initialConstraint2 = new Tcons0(Tcons0.EQ, new Texpr0Intern(new Texpr0BinNode(Texpr0BinNode.OP_SUB, b, ten)))
  val preState = state.meetCopy(manager, initialConstraint).meetCopy(manager, initialConstraint2)
  val postState = {
    val assignment = new Texpr0BinNode(Texpr0BinNode.OP_ADD, a, rStar)
    preState.assignCopy(manager, 1, new Texpr0Intern(assignment), null)
  }
  println(postState.toString(manager))
  val constraints: Array[Tcons0] = postState.toTcons(manager)
  constraints.foreach(c => println(c.toString))
  // apronState.meetCopy(apronManager.getManager, constraint)

  class State(abstractDomainName: AbstractDomainName) {
    private var variables = List[Variable]()
    private val manager = abstractDomainName match {
      case OCTAGON => new Octagon
      case POLKA => new Polka(false)
    }
    private var apronState = new Abstract0(manager, 0, 0)

    def declareNewVariable(variable: Variable): Unit = {
      variables = variable :: variables
      val dimensionChange = new Dimchange(1, 0, Array(0))
      apronState = apronState.addDimensionsCopy(manager, dimensionChange, false)
      assert(apronState != null)
    }

    def getVariable(variable: String): Texpr0Node = {
      val index = variables.indexWhere({ case Variable(name, _, _) => name == variable })
      if (index == -1) throw new Exception
      else new Texpr0DimNode(index)
    }
  }

  case class LexicalScope(block: Option[Statement], brboFunction: BrboFunction)

  case class Variable(name: String, typ: BrboType, scope: LexicalScope)

}

object AbstractMachine {
  private def createIntegerLiteral(value: Int): Texpr0Node = Texpr0Node.fromLinexpr0(new Linexpr0(new Array[Linterm0](0), new DoubleScalar(value)))

  private def generateConstraintEq(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.EQ, new Texpr0Intern(expression))

  private def generateConstraintGe(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.SUPEQ, new Texpr0Intern(expression))

  private def generateConstraintGt(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.SUP, new Texpr0Intern(expression))

  private def generateConstraintNe(expression: Texpr0Node): Tcons0 = new Tcons0(Tcons0.DISEQ, new Texpr0Intern(expression))
}
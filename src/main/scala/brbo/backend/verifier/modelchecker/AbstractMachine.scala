package brbo.backend.verifier.modelchecker

import apron._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.modelchecker.AbstractDomainName._
import brbo.backend.verifier.modelchecker.AbstractMachine.Variable
import brbo.backend.verifier._
import brbo.common.ast._
import brbo.common.cfg.{CFGNode, ControlFlowGraph}
import brbo.common.{CommandLineArguments, MyLogger, Z3Solver}
import org.apache.logging.log4j.LogManager

import scala.collection.immutable.Queue

class AbstractMachine(brboProgram: BrboProgram, arguments: CommandLineArguments) {
  private val logger: MyLogger = MyLogger(LogManager.getLogger(classOf[AbstractMachine]), arguments.getDebugMode)
  private val cfg = ControlFlowGraph.toControlFlowGraph(brboProgram)
  private val parentStatements = (brboProgram.mainFunction :: brboProgram.functions).foldLeft(Map[BrboAstNode, Statement]())({
    (acc, function) =>
      acc ++ BrboAstUtils.findParentStatements(function.actualBody)
  })
  private val manager = arguments.getAbstractDomain match {
    case OCTAGON => new Octagon
    case POLKA => new Polka(false)
  }

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

  private val fakeInitialNode = CFGNode(Right(Bool(b = true)), function = Some(brboProgram.mainFunction))

  def verify(boundAssertion: BoundAssertion): VerifierResult = {
    val initialState = State(fakeInitialNode, Valuation(), numberOfVisits = 1, shouldVerify = true)
    // Every CFGNode corresponds to a location, which is the location right after the node
    // fakeInitialNode corresponds to the program entry, which is right before the first command / expression
    val initialPathState = PathState(Map(initialState.node -> initialState.valuation))
    var reached = Map[List[CFGNode], PathState](Nil -> initialPathState)
    var waitlist: Queue[(List[CFGNode], State)] = Queue((Nil, initialState)) // Breadth First Search
    var stoppedEarly = false
    var counterexamplePath: Option[List[CFGNode]] = None
    while (waitlist.nonEmpty && counterexamplePath.isEmpty) {
      val ((path, state), newWaitlist) = waitlist.dequeue
      waitlist = newWaitlist
      val oldPathState = reached.getOrElse(path, throw new Exception)
      val newStates = step(state)
      newStates.foreach({
        newState =>
          if (counterexamplePath.isEmpty) {
            logger.traceOrError(s"New state: $newState")
            val refuted =
              if (newState.shouldVerify) {
                val refuted = !newState.valuation.satisfy(boundAssertion.assertion)
                logger.traceOrError(s"New state ${if (refuted) "does not satisfy" else "satisfies"} bound assertion `$boundAssertion`")
                refuted
              } else false
            if (refuted) {
              counterexamplePath = Some(path)
            }
            else {
              val (newValuation, keepExploring) = {
                oldPathState.valuations.get(newState.node) match {
                  case Some(existingValuation) =>
                    logger.traceOrError(s"Old valuation at node `${newState.node.prettyPrintToCFG}`: $existingValuation")
                    if (newState.valuation.include(existingValuation)) {
                      logger.traceOrError(s"The new valuation is included in the old one")
                      (existingValuation, false)
                    }
                    else {
                      logger.traceOrError(s"The new valuation is not included in the old one")
                      val joinedValuation = newState.valuation.joinCopy(existingValuation)
                      logger.traceOrError(s"Joined valuation: `$joinedValuation`")
                      if (newState.numberOfVisits <= arguments.getMaxPathLength) (joinedValuation, true)
                      else {
                        logger.traceOrError(s"Will stop exploring path: `$path`")
                        stoppedEarly = true
                        // TODO: Add this path to the counterexamples, so that it can be avoided in the future
                        (joinedValuation, false)
                      }
                    }
                  case None =>
                    logger.traceOrError(s"Old valuation at node `${newState.node.prettyPrintToCFG}` does not exist")
                    (newState.valuation, true)
                }
              }
              if (keepExploring) {
                val newValuations = oldPathState.valuations.updated(newState.node, newValuation)
                val newPath = newState.node :: path
                waitlist = waitlist.enqueue((newPath, State(newState.node, newValuation, newState.numberOfVisits, newState.shouldVerify)))
                reached = reached + (newPath -> PathState(newValuations))
              }
            }
          }
      })
      reached = reached - path
    }
    counterexamplePath match {
      case Some(path) =>
        VerifierResult(VerifierStatus.FALSE_RESULT, Some(Path(path)))
      case None =>
        if (stoppedEarly) VerifierResult(VerifierStatus.UNKNOWN_RESULT, None)
        else VerifierResult(VerifierStatus.TRUE_RESULT, None)
    }
  }

  def step(state: State): Set[State] = {
    // If the lexical scope of the next node is none or is different from the scope of the current node, then
    // forget all variables declared in the scope of the current node
    val nextNodes: Set[CFGNode] = {
      if (state.node == fakeInitialNode) Set(cfg.entryNode)
      else cfg.findSuccessorNodes(state.node)
    }
    ???
  }

  def interpret(valuation: Valuation, brboAstNode: BrboAstNode): Valuation = {
    brboAstNode match {
      case ast: BrboAst =>
        ast match {
          case command: Command => ???
          case _ => throw new Exception
        }
      case expr: BrboExpr =>
        val newStates = BrboExprUtils.toApron(expr, valuation.variablesNoScope) match {
          case Left(_) => throw new Exception
          case Right(constraint) => Apron.imposeConstraint(valuation.apronState, constraint)
        }
        if (newStates.size > 1)
          logger.traceOrError(s"Approximate the disjunction in `${expr.prettyPrintToCFG}` with a conjunctive domain")
        val joinedState = newStates.tail.foldLeft(newStates.head)({
          (acc, newState) => acc.joinCopy(manager, newState)
        })
        Valuation(valuation.variables, joinedState)
      case _ => throw new Exception
    }
  }

  case class Valuation(variables: List[Variable] = Nil,
                       apronState: Abstract0 = new Abstract0(manager, 0, 0)) {
    assert(apronState != null)
    logger.traceOrError(s"Create a new state. isTop: `${apronState.isTop(manager)}`. isBottom: `${apronState.isBottom(manager)}`")
    val variablesNoScope: List[Identifier] = variables.map(v => v.variable)

    def declareNewVariable(variable: Variable): Unit = {
      val newVariables = variable :: variables
      val dimensionChange = new Dimchange(1, 0, Array(0))
      val newApronState = apronState.addDimensionsCopy(manager, dimensionChange, false)
      Valuation(newVariables, newApronState)
    }

    def include(other: Valuation): Boolean = other.apronState.isIncluded(manager, apronState)

    def joinCopy(valuation: Valuation): Valuation = {
      val newApronState = apronState.joinCopy(manager, valuation.apronState)
      // TODO: Compare the two sets of variables?
      Valuation(variables, newApronState)
    }

    // def meetCopy(constraint: Tcons0): Valuation = Valuation(variables, apronState.meetCopy(manager, constraint))

    def assignCopy(variable: String, expression: Texpr0Node): Valuation = {
      val index = getVariableIndex(variable)
      val newState = apronState.assignCopy(manager, index, new Texpr0Intern(expression), null)
      Valuation(variables, newState)
    }

    def getVariableIndex(variable: String): Int = {
      variables.indexWhere({ case Variable(Identifier(identifier, _, _), _) => identifier == variable })
    }

    override def toString: String = {
      val variablesString = variables.map(v => v.toString).mkString("\n  ")
      val stateString = apronState.toString(manager, variables.map(v => v.variable.identifier).toArray)
      s"Variables:\n$variablesString\nApronState: $stateString"
    }

    def satisfy(constraint: Tcons0): Boolean = apronState.satisfy(manager, constraint)

    def satisfy(constraint: BrboExpr): Boolean = {
      val solver = new Z3Solver
      val state = apronState.toTcons(manager).map({
        constraint => Apron.constraintToZ3(constraint, solver, variablesNoScope)
      })
      val constraintZ3 = constraint.toZ3AST(solver)
      val query = {
        val variablesZ3 = variables.map(v => solver.mkDoubleVar(v.variable.identifier))
        solver.mkForall(variablesZ3, solver.mkImplies(solver.mkAnd(state: _*), constraintZ3))
      }
      solver.checkAssertionPushPop(query, arguments.getDebugMode)
    }
  }

  case class State(node: CFGNode, valuation: Valuation, numberOfVisits: Int, shouldVerify: Boolean) {
    override def toString: String = s"$toStringShort\nValuation: $valuation"

    def toStringShort: String = s"Node: `${node.prettyPrintToCFG}`. Number of visits: `$numberOfVisits`. shouldVerify: `$shouldVerify`."
  }

  case class PathState(valuations: Map[CFGNode, Valuation])
}

object AbstractMachine {

  case class LexicalScope(block: Option[Statement], brboFunction: BrboFunction)

  case class Variable(variable: Identifier, scope: LexicalScope)
}
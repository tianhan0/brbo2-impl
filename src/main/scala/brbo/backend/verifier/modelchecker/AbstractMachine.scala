package brbo.backend.verifier.modelchecker

import apron._
import brbo.backend.driver.Driver
import brbo.backend.verifier._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.modelchecker.AbstractDomainName._
import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.Apron.{ApronVariable, Constraint, Singleton}
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
    val initialState = State(fakeInitialNode, createEmptyValuation(manager), numberOfVisits = 1, shouldVerify = true)
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

  def evalCommand(valuation: Valuation, brboAst: BrboAst): Valuation = {
    val scope = parentStatements.get(brboAst)

    def evalExprHelper(expr: BrboExpr): (ApronVariable, Valuation) = {
      val (apronRepr, newValuation) = BrboExprUtils.toApron(expr, valuation, scope)
      (apronRepr.asInstanceOf[ApronVariable], newValuation)
    }

    brboAst match {
      case command: Command =>
        command match {
          case VariableDeclaration(variable, initialValue, _) =>
            val (apronVariable, newValuation) = evalExprHelper(initialValue)
            val newValuation2 = newValuation.declareNewVariable(Variable(variable, scope))
            val constraint = Singleton(Apron.mkEq(Apron.mkSub(???, Apron.mkVar(apronVariable.index))))
            newValuation2.imposeConstraint(constraint)
          case Assignment(variable, expression, _) => ???
          case _: CFGOnly => valuation
          case Return(value, _) => ???
          case Use(groupId, update, condition, _) => ???
          case Reset(groupId, condition, _) =>
            // For r*, interpret as joining the post states of r* = r and r* = r*, instead of r* = r*>=r ? r* : r, because
            // (1) we only want to learn paths (as root causes), and
            // (2) this is sound and precise for the bound verification
            ???
          case FunctionCall(functionCallExpr, _) => ???
          case LabeledCommand(_, command, _) => evalCommand(valuation, command)
          case _: CexPathOnly => throw new Exception
          case Skip(_) => valuation
          case _@(Break(_) | Continue(_)) => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}

object AbstractMachine {
  // private val logger = MyLogger.createLogger(AbstractMachine.getClass, debugMode = false)

  case class State(node: CFGNode, valuation: Valuation, numberOfVisits: Int, shouldVerify: Boolean) {
    override def toString: String = s"$toStringShort\nValuation: $valuation"

    def toStringShort: String = s"Node: `${node.prettyPrintToCFG}`. Number of visits: `$numberOfVisits`. shouldVerify: `$shouldVerify`."
  }

  case class PathState(valuations: Map[CFGNode, Valuation])

  case class Valuation(variables: List[Variable], apronState: Abstract0, logger: Option[MyLogger] = None) {
    assert(apronState != null)
    private val manager = apronState.getCreationManager
    assert(apronState.getDimension(manager).intDim == variables.size)
    traceOrError(logger, s"Create a new state. isTop: `${apronState.isTop(manager)}`. isBottom: `${apronState.isBottom(manager)}`")
    private val debugMode = logger match {
      case Some(logger) => logger.debugMode
      case None => false
    }
    val variablesNoScope: List[Identifier] = variables.map(v => v.variable)

    def declareNewVariable(variable: Variable): Valuation = {
      val newVariables = variable :: variables
      val dimensionChange = new Dimchange(1, 0, Array(variables.size))
      val newApronState = apronState.addDimensionsCopy(manager, dimensionChange, false)
      Valuation(newVariables, newApronState, logger)
      // TODO: Test this
    }

    def forgetVariablesInScope(scope: Statement): Valuation = {
      // apronState.forgetCopy()
      ???
    }

    def include(other: Valuation): Boolean = other.apronState.isIncluded(manager, apronState)

    def joinCopy(valuation: Valuation): Valuation = {
      val newApronState = apronState.joinCopy(manager, valuation.apronState)
      // TODO: Compare the two sets of variables?
      Valuation(variables, newApronState, logger)
    }

    // def meetCopy(constraint: Tcons0): Valuation = Valuation(variables, apronState.meetCopy(manager, constraint))

    def assignCopy(variable: String, expression: Texpr0Node): Valuation = {
      val index = getVariableIndex(variable)
      val newState = apronState.assignCopy(manager, index, new Texpr0Intern(expression), null)
      Valuation(variables, newState, logger)
    }

    def getVariableIndex(variable: String): Int = {
      variables.indexWhere({ case Variable(Identifier(identifier, _, _), _) => identifier == variable })
    }

    override def toString: String = {
      val variablesString = "  " + variables.map(v => v.toString).mkString("\n  ")
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
      solver.checkAssertionPushPop(query, debugMode)
    }

    def imposeConstraint(constraint: Constraint): Valuation = {
      val newStates = Apron.imposeConstraint(apronState, constraint)
      if (newStates.size > 1)
        traceOrError(logger, s"Approximate the disjunction in `$constraint` with a conjunctive domain")
      val joinedState = newStates.tail.foldLeft(newStates.head)({
        (acc, newState) => acc.joinCopy(manager, newState)
      })
      Valuation(variables, joinedState, logger)
    }
  }

  case class Variable(variable: Identifier, scope: Option[Statement])

  /**
   *
   * @param block        The statement that defines the current scope
   * @param brboFunction The function that defines the statement
   */
  case class LexicalScope(block: Option[Statement], brboFunction: BrboFunction)

  def createEmptyValuation(manager: Manager, logger: Option[MyLogger] = None): Valuation = {
    val state = new Abstract0(manager, 0, 0)
    Valuation(Nil, state, logger)
  }

  private def traceOrError(logger: Option[MyLogger], message: String): Unit = {
    logger match {
      case Some(logger) => logger.traceOrError(message)
      case None =>
    }
  }
}
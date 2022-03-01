package brbo.backend.verifier.modelchecker

import apron._
import brbo.backend.verifier._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.modelchecker.AbstractDomainName._
import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.Apron._
import brbo.common.ast._
import brbo.common.cfg.{CFGNode, ControlFlowGraph}
import brbo.common.{CommandLineArguments, MyLogger}
import org.apache.logging.log4j.LogManager

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class AbstractMachine(brboProgram: BrboProgram, arguments: CommandLineArguments) {
  private val logger: MyLogger = MyLogger(LogManager.getLogger(classOf[AbstractMachine]), arguments.getDebugMode)
  private val cfg = ControlFlowGraph.toControlFlowGraph(brboProgram)
  private val parentStatements: Map[BrboAstNode, Statement] =
    (brboProgram.mainFunction :: brboProgram.functions).foldLeft(Map[BrboAstNode, Statement]())({
      (acc, function) => acc ++ BrboAstUtils.findParentStatements(function.actualBody)
    })
  private val manager = arguments.getAbstractDomain match {
    case OCTAGON => new Octagon
    case POLKA_STRICT => new Polka(true)
    case POLKA_NONSTRICT => new Polka(false)
  }

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
                val refuted = !newState.satisfy(boundAssertion.assertion)
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
      // Stop if the state becomes bottom
    }
    ???
  }
}

object AbstractMachine {
  // private val logger = MyLogger.createLogger(AbstractMachine.getClass, debugMode = false)

  /**
   *
   * @param valuation The valuation under which the command will be evaluated
   * @param brboAst   The command to evaluate
   * @param scope     The lexical scope of the given command
   * @return
   */
  @tailrec
  def evalCommand(valuation: Valuation, brboAst: BrboAst,
                  scope: Option[Statement], logger: Option[MyLogger] = None): Valuation = {
    brboAst match {
      case command: Command =>
        command match {
          case VariableDeclaration(identifier, initialValue, _) =>
            assign(identifier, initialValue, createNewVariable = true, valuation, scope)
          case Assignment(identifier, expression, _) =>
            assign(identifier, expression, createNewVariable = false, valuation, scope)
          case _: CFGOnly => valuation
          case use@Use(_, update, condition, _) =>
            traceOrError(logger, s"Evaluating `${use.prettyPrintToCFG}`")
            val updatedValuation =
              assign(use.resourceVariable, Addition(use.resourceVariable, update), createNewVariable = false, valuation, scope)
            traceOrError(logger, s"Updated valuation: `$updatedValuation`")
            evalGhostCommandHelper(condition, valuation.satisfy(condition), valuation.satisfy(Negation(condition)),
              updatedValuation, valuation, logger)
          case reset@Reset(_, condition, _) =>
            traceOrError(logger, s"Evaluating `${reset.prettyPrintToCFG}`")
            val updatedCounterVariable = {
              val updatedResourceVariable = {
                val updatedStarVariable = {
                  // For r*, interpret as joining the post states of r* = r and r* = r*, instead of r* = r*>=r ? r* : r, because
                  // (1) we only want to learn paths (as root causes), and
                  // (2) this is sound and precise for the bound verification
                  val v = assign(reset.starVariable, reset.resourceVariable, createNewVariable = false, valuation, scope)
                  traceOrError(logger, s"After updating r* (before join): `$v`")
                  v.joinCopy(valuation)
                }
                traceOrError(logger, s"After updating r* (after join): `$updatedStarVariable`")
                assign(reset.resourceVariable, Number(0), createNewVariable = false, updatedStarVariable, scope)
              }
              traceOrError(logger, s"After updating r: `$updatedResourceVariable`")
              assign(reset.counterVariable, Addition(reset.counterVariable, Number(1)), createNewVariable = false, updatedResourceVariable, scope)
            }
            traceOrError(logger, s"After updating r#: `$updatedCounterVariable`")
            evalGhostCommandHelper(condition, valuation.satisfy(condition), valuation.satisfy(Negation(condition)),
              updatedCounterVariable, valuation, logger)
          case Return(_, _) => throw new Exception
          case FunctionCall(_, _) => throw new Exception
          case LabeledCommand(_, command, _) => evalCommand(valuation, command, scope)
          case _: CexPathOnly => throw new Exception
          case Skip(_) => valuation
          case _@(Break(_) | Continue(_)) => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  private def evalGhostCommandHelper(condition: BrboExpr, allThen: Boolean, allElse: Boolean,
                                     thenValuation: Valuation, elseValuation: Valuation, logger: Option[MyLogger]): Valuation = {
    traceOrError(logger, s"All states satisfy `${condition.prettyPrintToCFG}`? `$allThen`")
    traceOrError(logger, s"All states satisfy `${Negation(condition).prettyPrintToCFG}`? `$allElse`")
    if (allThen) thenValuation
    else {
      if (allElse) elseValuation
      else {
        /**
         * This situation means the current abstraction is not precise enough to decide
         * if the condition is satisfied on the path that leads to this situation.
         *
         * To ensure soundness, we decide to respect such imprecision as executing both branches
         * at the same time, which however may lead to verification failures. For example,
         * a single use command may be interpreted as executing two use commands for two groups.
         *
         * By respecting such imprecision, we can ensure not choosing a candidate program that
         * contains a path that leads to this situation.
         */
        thenValuation.joinCopy(elseValuation)
      }
    }
  }

  private def assign(identifier: Identifier, expr: BrboExpr, createNewVariable: Boolean,
                     valuation: Valuation, scope: Option[Statement]): Valuation = {
    val (apronRepr, newValuation) = BrboExprUtils.toApron(expr, valuation, scope)
    val variable = Variable(identifier, scope)
    val newValuation2 =
      if (createNewVariable) newValuation.createInitializedVariable(variable)
      else newValuation
    apronRepr match {
      case Apron.ApronExpr(node) => newValuation2.assignCopy(variable, node)
      case constraint: Constraint =>
        // TODO: This approximates the value, as opposed to exactly representing the value,
        //  because we represent boolean-typed values as constraints
        val value =
          if (newValuation2.satisfy(constraint)) BOOLEAN_POSITIVE
          else BOOLEAN_NEGATIVE
        newValuation2.assignCopy(variable, Apron.mkIntVal(value))
      case _ => throw new Exception
    }
  }

  case class State(node: CFGNode, valuation: Valuation, numberOfVisits: Int, shouldVerify: Boolean) {
    val scope: Option[Statement] = {
      node.value match {
        case Left(value) => valuation.lookupScope(value)
        case Right(value) => valuation.lookupScope(value)
      }
    }

    def satisfy(brboExpr: BrboExpr): Boolean = valuation.satisfy(brboExpr)

    override def toString: String = s"$toStringShort\nValuation: $valuation"

    def toStringShort: String = s"Node: `${node.prettyPrintToCFG}`. Number of visits: `$numberOfVisits`. shouldVerify: `$shouldVerify`."
  }

  case class PathState(valuations: Map[CFGNode, Valuation])

  /**
   *
   * @param variables    All variables that can be looked up or re-assigned in the current program state
   * @param apronState   The abstraction of the current program state
   * @param allVariables All variables that have been declared in apronState
   * @param lookupScope  A function to look up the parent scope of a given scope
   * @param logger       The logger
   */
  case class Valuation(variables: List[Variable],
                       apronState: Abstract0,
                       allVariables: Set[Variable],
                       lookupScope: BrboAstNode => Option[Statement],
                       logger: Option[MyLogger] = None) {
    assert(apronState != null)
    private val manager = apronState.getCreationManager
    private val numberOfIntNumbers = apronState.getDimension(manager).intDim
    private val numberOfFloatNumbers = apronState.getDimension(manager).realDim
    assert(numberOfIntNumbers + numberOfFloatNumbers == variables.size)
    traceOrError(logger, s"Create a new state. isTop: `${apronState.isTop(manager)}`. isBottom: `${apronState.isBottom(manager)}`")
    private val debugMode = logger match {
      case Some(logger) => logger.debugMode
      case None => false
    }
    val variablesNoScope: List[Identifier] = variables.map(v => v.identifier)

    def indexOfVariableThisScope(variable: Variable): Int =
      variables.indexWhere(v => v.identifier.sameAs(variable.identifier) && v.scope == variable.scope)

    @tailrec
    final def indexOfVariableAnyScope(variable: Variable): Int = {
      val index = indexOfVariableThisScope(variable)
      if (index != -1) index
      else {
        variable.scope match {
          case Some(parentScope) => indexOfVariableAnyScope(Variable(variable.identifier, lookupScope(parentScope)))
          case None => -1
        }
      }
    }

    def apronVariableThisScope(variable: Variable): Texpr0Node = Apron.mkVar(indexOfVariableThisScope(variable))

    def apronVariableAnyScope(variable: Variable): Texpr0Node = Apron.mkVar(indexOfVariableAnyScope(variable))

    // The variable is initialized as 0
    def createInitializedVariable(variable: Variable): Valuation = {
      val index = indexOfVariableThisScope(variable)
      if (index == -1) {
        createVariableHelper(variable, initialize = true)
      }
      else {
        // Re-creating the variable means initializing to 0
        assignCopy(variable, Apron.mkIntVal(0))
      }
    }

    def createUninitializedVariable(variable: Variable): Valuation = {
      val index = indexOfVariableThisScope(variable)
      if (index == -1) {
        createVariableHelper(variable, initialize = false)
      }
      else {
        throw new Exception
      }
    }

    private def createVariableHelper(variable: Variable, initialize: Boolean): Valuation = {
      val index = allVariables.size
      val dimensionChange = {
        variable.identifier.typ match {
          case brbo.common.BrboType.INT | brbo.common.BrboType.BOOL =>
            new Dimchange(1, 0, Array(index))
          // new Dimchange(0, 1, Array(index))
          case brbo.common.BrboType.FLOAT => new Dimchange(0, 1, Array(index))
          case _ => throw new Exception
        }
      }
      val newApronState = apronState.addDimensionsCopy(manager, dimensionChange, initialize)
      Valuation(variables :+ variable, newApronState, allVariables + variable, lookupScope, logger)
    }

    /*def forgetVariablesInScope(scope: Statement): Valuation = {
      val withIndex = variables.zipWithIndex
      val indices = withIndex.filter({
        case (v, _) => v.scope match {
              case Some(scope2) => scope2 == scope
              case None => false
            }
      }).map(pair => pair._2).toArray
      val newState = apronState.forgetCopy(manager, indices, false)
      val newVariables = withIndex.foldLeft(Nil: List[Variable])({
        case (acc, (v, index)) =>
          if (indices.contains(index)) acc
          else v :: acc
      }).reverse
      Valuation(newVariables, newState, logger)
    }*/

    def include(other: Valuation): Boolean = other.apronState.isIncluded(manager, apronState)

    def joinCopy(valuation: Valuation): Valuation = {
      val newApronState = apronState.joinCopy(manager, valuation.apronState)
      assert(valuation.variables == variables)
      assert(valuation.allVariables == allVariables)
      Valuation(variables, newApronState, allVariables, lookupScope, logger)
    }

    def assignCopy(variable: Variable, expression: Texpr0Node): Valuation = {
      val index = indexOfVariableAnyScope(variable)
      if (index == -1) this
      else {
        val newState = apronState.assignCopy(manager, index, new Texpr0Intern(expression), null)
        Valuation(variables, newState, allVariables, lookupScope, logger)
      }
    }

    override def toString: String = {
      val variablesString = "  " + variables.map(v => v.toString).mkString("\n  ")
      val stateString = apronState.toString(manager) //, variables.map(v => v.variable.identifier).toArray)
      s"Variables:\n$variablesString\nApronState: $stateString"
    }

    def satisfy(constraint: Constraint): Boolean = {
      constraint match {
        case Apron.Conjunction(left, right) => satisfy(left) && satisfy(right)
        case Apron.Disjunction(left, right) => satisfy(left) || satisfy(right)
        case Singleton(constraint) => satisfy(constraint)
        case _ => throw new Exception
      }
    }

    def satisfy(constraint: Tcons0): Boolean = {
      error(logger, s"Check satisfaction for constraint: $constraint")
      apronState.satisfy(manager, constraint)
    }

    def satisfy(constraint: BrboExpr): Boolean = {
      val (apronRepr, newValuation) = BrboExprUtils.toApron(constraint, valuation = this, scope = None)
      apronRepr match {
        case constraint: Constraint => newValuation.satisfy(constraint)
        case _ => throw new Exception
      }
      // Seems to not work correctly if translating to Z3
      /*val solver = new Z3Solver
      val state = apronState.toTcons(manager).map({
        constraint => Apron.constraintToZ3(constraint, solver, variablesNoScope)
      })
      val constraintZ3 = constraint.toZ3AST(solver)
      val query = {
        val variablesZ3 = allVariables.map(v => solver.mkDoubleVar(v.identifier.name))
        solver.mkForall(variablesZ3, solver.mkImplies(solver.mkAnd(state: _*), constraintZ3))
      }
      // logger.get.error(s"query: $query")
      traceOrError(logger, s"query: $query")
      solver.checkAssertionPushPop(query, debugMode)*/
    }

    def imposeConstraint(constraint: Constraint): Valuation = {
      val newStates = Apron.imposeConstraint(apronState, constraint)
      if (newStates.size > 1)
        traceOrError(logger, s"Approximate the disjunction in `$constraint` with a conjunctive domain")
      val joinedState = newStates.tail.foldLeft(newStates.head)({
        (acc, newState) => acc.joinCopy(manager, newState)
      })
      Valuation(variables, joinedState, allVariables, lookupScope, logger)
    }

    def toBottom(): Valuation = imposeConstraint(Singleton(Apron.mkEqZero(Apron.mkIntVal(1))))
  }

  case class Variable(identifier: Identifier, scope: Option[Statement])

  def createEmptyValuation(manager: Manager,
                           lookupScope: Option[BrboAstNode => Option[Statement]] = None,
                           logger: Option[MyLogger] = None): Valuation = {
    val state = new Abstract0(manager, 0, 0)
    lookupScope match {
      case Some(lookupScope) => Valuation(Nil, state, Set(), lookupScope, logger)
      case None => Valuation(Nil, state, Set(), _ => None, logger)
    }
  }

  private def traceOrError(logger: Option[MyLogger], message: String): Unit = {
    logger match {
      case Some(logger) => logger.traceOrError(message)
      case None =>
    }
  }

  private def error(logger: Option[MyLogger], message: String): Unit = {
    logger match {
      case Some(logger) => logger.traceOrError(message)
      case None =>
    }
  }
}
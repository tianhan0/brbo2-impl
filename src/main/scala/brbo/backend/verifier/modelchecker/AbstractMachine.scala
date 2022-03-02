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
  private val fakeInitialNode = CFGNode(Bool(b = true), function = Some(brboProgram.mainFunction))
  private val manager = arguments.getAbstractDomain match {
    case OCTAGON => new Octagon
    case POLKA_STRICT => new Polka(true)
    case POLKA_NONSTRICT => new Polka(false)
  }

  private val initialValuation = {
    val parentStatements: Map[BrboAstNode, Statement] =
      (brboProgram.mainFunction :: brboProgram.functions)
        .foldLeft(Map[BrboAstNode, Statement](fakeInitialNode.value -> brboProgram.mainFunction.actualBody))({
          (acc, function) => acc ++ BrboAstUtils.findParentStatements(function.actualBody)
        })
    val scopeOperations = new ScopeOperations(parentStatements)
    val v = createEmptyValuation(manager, Some(logger), scopeOperations)
    brboProgram.mainFunction.parameters.foldLeft(v)({
      (acc, v) => acc.createUninitializedVariable(Variable(v, None))
    })
  }

  def verify(assertion: BrboExpr): VerifierResult = {
    val initialState = State(fakeInitialNode, initialValuation, indexOnPath = 0, shouldVerify = true)
    // Every CFGNode corresponds to a location, which is the location right after the node
    // fakeInitialNode corresponds to the program entry, which is right before the first command / expression
    val initialPathState = PathState(Map(initialState.node -> initialState.valuation))
    var reached = Map[List[CFGNode], PathState](Nil -> initialPathState)
    var waitlist: Queue[(List[CFGNode], State)] = Queue((Nil, initialState)) // Breadth First Search
    var stoppedEarly = false
    var counterexamplePaths: Set[List[CFGNode]] = Set()
    var refuted = false
    var exploredPaths: List[List[CFGNode]] = List()
    while (waitlist.nonEmpty && counterexamplePaths.isEmpty) {
      val ((path, state), newWaitlist) = waitlist.dequeue
      waitlist = newWaitlist
      val oldPathState: PathState = reached.getOrElse(path, throw new Exception)
      logger.traceOrError(s"[model check] Dequeued path: `$path`")
      logger.traceOrError(s"[model check] Dequeued state: `${state.toShortString}`")
      val newStates = step(state)
      newStates.foreach({
        newState =>
          logger.traceOrError(s"[model check] New state: ${newState.toShortString}")
          val newPath = newState.node :: path
          exploredPaths = newPath :: exploredPaths
          if (!refuted && !newState.valuation.isBottom()) {
            val refuted2 =
              if (newState.shouldVerify) {
                val refuted = !newState.satisfy(assertion)
                logger.traceOrError(s"[model check] New state ${if (refuted) "does not satisfy" else "satisfies"} bound assertion `$assertion`")
                if (refuted)
                  logger.traceOrError(s"[model check] Bound violation state: `${newState.toShortString}`")
                refuted
              } else false
            if (refuted2) {
              counterexamplePaths = counterexamplePaths + newPath
              refuted = true
            }
            else {
              val (newValuation, keepExploring) = {
                oldPathState.valuations.get(newState.node) match {
                  case Some(existingValuation) =>
                    logger.traceOrError(s"[model check] Old valuation at node `${newState.node.prettyPrintToCFG}`: $existingValuation")
                    if (newState.valuation.include(existingValuation)) {
                      logger.traceOrError(s"[model check] The new valuation is included in the old one")
                      (existingValuation, false)
                    }
                    else {
                      logger.traceOrError(s"[model check] The new valuation is not included in the old one")
                      val joinedValuation = newState.valuation.joinCopy(existingValuation)
                      logger.traceOrError(s"[model check] Joined valuation: `$joinedValuation`")
                      if (newState.indexOnPath <= arguments.getMaxPathLength) (joinedValuation, true)
                      else {
                        logger.traceOrError(s"[model check] Will stop exploring path due to reaching the max path length: `$newPath`")
                        stoppedEarly = true
                        // Add this path to the counterexamples, so that it can be avoided in the future
                        counterexamplePaths = counterexamplePaths + newPath
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
                waitlist = waitlist.enqueue((newPath, State(newState.node, newValuation, newState.indexOnPath, newState.shouldVerify)))
                reached = reached + (newPath -> PathState(newValuations))
              }
            }
          }
      })
      reached = reached - path
    }
    logger.traceOrError(s"Explored `${exploredPaths.size}` paths")
    logger.traceOrError(s"\n${exploredPaths.map(p => p.map(n => n.simplifiedString).reverse).reverse.mkString("  \n")}")
    val paths = counterexamplePaths.map(p => Path(p))
    if (refuted) VerifierResult(VerifierStatus.FALSE_RESULT, paths)
    else {
      if (stoppedEarly) VerifierResult(VerifierStatus.UNKNOWN_RESULT, paths)
      else VerifierResult(VerifierStatus.TRUE_RESULT, paths)
    }
  }

  def step(state: State): Set[State] = {
    logger.traceOrError(s"[step] Current state: `${state.toShortString}`")
    val scopeOperations = state.valuation.scopeOperations
    val currentScope = scopeOperations.getScope(state.node.value)
    val nextNodes: Set[(CFGNode, State)] = {
      val currentNode = state.node
      if (currentNode == fakeInitialNode) Set((cfg.entryNode, state))
      else {
        val nextNodes = cfg.findSuccessorNodes(currentNode)
        nextNodes.size match {
          case 0 => Set()
          case 1 => Set((nextNodes.head, state))
          case 2 =>
            val n1 = nextNodes.head
            val n2 = nextNodes.tail.head
            val e1 = cfg.jgraphtGraph.getEdge(currentNode, n1)
            val e2 = cfg.jgraphtGraph.getEdge(currentNode, n2)
            val trueCondition = currentNode.value.asInstanceOf[BrboExpr]
            val falseCondition = Negation(trueCondition)
            val trueState = {
              val v = evalExpr(state.valuation, trueCondition, currentScope)
              State(currentNode, v, state.indexOnPath, state.shouldVerify)
            }
            val falseState = {
              val v = evalExpr(state.valuation, falseCondition, currentScope)
              State(currentNode, v, state.indexOnPath, state.shouldVerify)
            }
            (cfg.jgraphtGraph.getEdgeWeight(e1), cfg.jgraphtGraph.getEdgeWeight(e2)) match {
              case (ControlFlowGraph.TRUE_BRANCH_WEIGHT, ControlFlowGraph.FALSE_BRANCH_WEIGHT) =>
                Set((n1, trueState), (n2, falseState))
              case (ControlFlowGraph.FALSE_BRANCH_WEIGHT, ControlFlowGraph.TRUE_BRANCH_WEIGHT) =>
                Set((n1, falseState), (n2, trueState))
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
      }
    }
    logger.traceOrError(s"[step] Current scope: `$currentScope`")
    nextNodes.map({
      case (nextNode, state) =>
        logger.traceOrError(s"[step] Next node: `${nextNode.prettyPrintToCFG}`")
        val nextScope = scopeOperations.getScope(nextNode.value)
        logger.traceOrError(s"[step] Next scope: `$nextScope`")
        val valuation = {
          val valuation = {
            nextNode.value match {
              case _: BrboExpr => state.valuation // Postpone the evaluation of expressions until branching into two nodes
              case command: Command => AbstractMachine.evalCommand(state.valuation, command, nextScope, Some(logger))
              case _ => throw new Exception
            }
          }
          if (scopeOperations.isUnknown(currentScope) || scopeOperations.isUnknown(nextScope)
            || scopeOperations.isSubScope(nextScope, currentScope)) {
            logger.traceOrError(s"[step] Current or next scope is unknown, or next scope is a (non-strict) sub-scope of the current scope")
            valuation
          }
          else {
            logger.traceOrError(s"[step] Next scope is not a (non-strict) sub-scope of the current scope")
            valuation.removeVariablesInScope(currentScope)
          }
        }
        val shouldVerify = nextNode.value match {
          case _: Use => true // Only need to verify a state after executing a use command
          case _ => false
        }
        State(nextNode, valuation, state.indexOnPath + 1, shouldVerify)
    })
  }
}

object AbstractMachine {
  // private val logger = MyLogger.createLogger(AbstractMachine.getClass, debugMode = false)

  /**
   *
   * @param valuation     The valuation under which the command or the expression will be evaluated
   * @param command The command or the expression to evaluate
   * @param scope         The lexical scope of the given command
   * @return
   */
  @tailrec
  def evalCommand(valuation: Valuation, command: Command,
                  scope: Scope, logger: Option[MyLogger] = None): Valuation = {
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
        traceOrError(logger, s"Updated valuation: `${updatedValuation.toShortString}`")
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
              traceOrError(logger, s"After updating r* (before join): `${v.toShortString}`")
              v.joinCopy(valuation)
            }
            traceOrError(logger, s"After updating r* (after join): `${updatedStarVariable.toShortString}`")
            assign(reset.resourceVariable, Number(0), createNewVariable = false, updatedStarVariable, scope)
          }
          traceOrError(logger, s"After updating r: `${updatedResourceVariable.toShortString}`")
          assign(reset.counterVariable, Addition(reset.counterVariable, Number(1)), createNewVariable = false, updatedResourceVariable, scope)
        }
        traceOrError(logger, s"After updating r#: `${updatedCounterVariable.toShortString}`")
        evalGhostCommandHelper(condition, valuation.satisfy(condition), valuation.satisfy(Negation(condition)),
          updatedCounterVariable, valuation, logger)
      case Return(_, _) => throw new Exception
      case FunctionCall(_, _) => throw new Exception
      case LabeledCommand(_, command, _) => evalCommand(valuation, command, scope)
      case _: CexPathOnly => throw new Exception
      case Skip(_) => valuation
      case _@(Break(_) | Continue(_)) => throw new Exception
      case Assume(condition, _) => evalExpr(valuation, condition, scope)
    }
  }

  def evalExpr(valuation: Valuation, brboExpr: BrboExpr, scope: Scope): Valuation = {
    val (apronRepr, newValuation) = BrboExprUtils.toApron(brboExpr, valuation, scope)
    apronRepr match {
      case constraint: Constraint => newValuation.imposeConstraint(constraint)
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
                     valuation: Valuation, scope: Scope): Valuation = {
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

  trait ToShortString {
    def toShortString: String
  }

  case class State(node: CFGNode, valuation: Valuation, indexOnPath: Int, shouldVerify: Boolean) extends ToShortString {
    val scope: Scope = valuation.scopeOperations.getScope(node.value)

    def satisfy(brboExpr: BrboExpr): Boolean = valuation.satisfy(brboExpr)

    def toShortString: String = s"Node: `${node.prettyPrintToCFG}`. Number of visits: `$indexOnPath`. shouldVerify: `$shouldVerify`. Valuation: ${valuation.toShortString}"
  }

  case class PathState(valuations: Map[CFGNode, Valuation])

  /**
   *
   * @param variables       All variables that can be looked up or re-assigned in the current program state
   * @param apronState      The abstraction of the current program state
   * @param allVariables    All variables that have been declared in apronState
   * @param scopeOperations Functions to manipulate scopes
   * @param logger          The logger
   */
  case class Valuation(variables: List[Variable],
                       apronState: Abstract0,
                       allVariables: Set[Variable],
                       scopeOperations: ScopeOperations,
                       logger: Option[MyLogger] = None) extends ToShortString {
    assert(apronState != null)
    private val manager = apronState.getCreationManager
    // traceOrError(logger, s"Create a new state. isTop: `${apronState.isTop(manager)}`. isBottom: `${apronState.isBottom(manager)}`")
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
          case Some(parentScope) => indexOfVariableAnyScope(Variable(variable.identifier, scopeOperations.getScope(parentScope)))
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
        throw new Exception("When creating uninitialized variables, the names must be fresh")
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
      Valuation(variables :+ variable, newApronState, allVariables + variable, scopeOperations, logger)
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
      Valuation(variables, newApronState, allVariables, scopeOperations, logger)
    }

    def assignCopy(variable: Variable, expression: Texpr0Node): Valuation = {
      val index = indexOfVariableAnyScope(variable)
      if (index == -1) this
      else {
        val newState = apronState.assignCopy(manager, index, new Texpr0Intern(expression), null)
        Valuation(variables, newState, allVariables, scopeOperations, logger)
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
      // error(logger, s"Check satisfaction for constraint: $constraint")
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
      Valuation(variables, joinedState, allVariables, scopeOperations, logger)
    }

    def toBottom(): Valuation = imposeConstraint(Singleton(Apron.mkEqZero(Apron.mkIntVal(1))))

    def isBottom(): Boolean = apronState.isBottom(manager)

    def removeVariablesInScope(scope: Scope): Valuation = {
      val firstIndex = variables.indexWhere(v => scopeOperations.isSame(v.scope, scope))
      if (firstIndex == -1) this
      else {
        val (toKeep, toRemove) = variables.splitAt(firstIndex)
        traceOrError(logger, s"Removing variables in scope `$scope` from variables `$variables`")
        traceOrError(logger, s"toRemove: `$toRemove`")
        assert(toRemove.forall(v => v.scope == scope))
        Valuation(toKeep, apronState, allVariables, scopeOperations, logger)
      }
    }

    override def toShortString: String = {
      s"Variables: `${variables.map(v => v.toShortString)}`. ApronState: `$apronState`"
    }
  }

  case class Variable(identifier: Identifier, scope: Scope) extends ToShortString {
    override def toShortString: String = identifier.toString
  }

  type Scope = Option[Statement]
  val TOP_SCOPE: Scope = Some(Block(Nil))
  val UNKNOWN_SCOPE: Scope = None
  val DEFAULT_SCOPE_OPERATIONS = new ScopeOperations(Map())

  class ScopeOperations(parentStatements: Map[BrboAstNode, Statement]) {
    def getScope(node: BrboAstNode): Scope = parentStatements.get(node)

    def isSubScope(child: Scope, parent: Scope): Boolean = isStrictSubScope(child, parent) || isSame(child, parent)

    def isStrictSubScope(child: Scope, parent: Scope): Boolean = {
      if (isUnknown(child) || isUnknown(parent) || isSame(child, parent)) false
      else {
        if (isTop(child)) false
        else {
          if (isTop(parent)) true
          else {
            val childScope = child.get
            val scopeOfChild = getScope(childScope)
            if (isSame(scopeOfChild, parent)) true
            else isSubScope(scopeOfChild, parent)
          }
        }
      }
    }

    def isSame(scope1: Scope, scope2: Scope): Boolean = scope1 == scope2

    def isTop(scope: Scope): Boolean = isSame(scope, TOP_SCOPE)

    def isUnknown(scope: Scope): Boolean = isSame(scope, UNKNOWN_SCOPE)
  }

  def createEmptyValuation(manager: Manager,
                           logger: Option[MyLogger],
                           scopeOperations: ScopeOperations = DEFAULT_SCOPE_OPERATIONS): Valuation = {
    val state = new Abstract0(manager, 0, 0)
    Valuation(Nil, state, Set(), scopeOperations, logger)
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
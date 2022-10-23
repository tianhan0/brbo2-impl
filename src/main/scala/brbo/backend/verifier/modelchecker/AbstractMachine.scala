package brbo.backend.verifier.modelchecker

import apron._
import brbo.backend.verifier._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.modelchecker.AbstractDomainName._
import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.Apron._
import brbo.common.ast.BrboExprUtils.{imply, greaterThan, greaterThanOrEqualTo, lessThanOrEqualTo}
import brbo.common.ast._
import brbo.common.cfg.{CFGNode, ControlFlowGraph}
import brbo.common.string.StringFormatUtils
import brbo.common.{CommandLineArguments, MyLogger, Z3Solver}
import com.microsoft.z3.AST
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
  private val debugJoinWiden: Boolean = arguments.getDebugMode
  private val debugBottom: Boolean = false

  private val initialValuation = {
    val parentStatements: Map[BrboAst, Statement] =
      (brboProgram.mainFunction :: brboProgram.functions)
        .foldLeft(Map[BrboAst, Statement](fakeInitialNode.command -> brboProgram.mainFunction.actualBody))({
          (acc, function) => acc ++ BrboAstUtils.findParentStatements(function.actualBody)
        })
    val scopeOperations = new ScopeOperations(parentStatements)
    val v = createEmptyValuation(manager, Some(logger), scopeOperations)
    brboProgram.mainFunction.parameters.foldLeft(v)({
      (acc, v) => acc.createUninitializedVariable(Variable(v, None))
    })
  }
  private val inputsArePositive = brboProgram.mainFunction.parameters.map(i => greaterThan(i, Number(0))).foldLeft(Bool(b = true): BrboExpr)({
    (acc, gt) => And(acc, gt)
  })
  private var firstRun = true

  /**
   *
   * @param assertion     The assertion to verify
   * @param maxPathLength The max length of any path that we can explore
   * @return
   */
  def verify(assertion: BrboExpr, solver: Option[Z3Solver], maxPathLength: Int = arguments.getMaxPathLength): Result = {
    if (!firstRun) throw new Exception(s"Every instance of AbstractMachine can only run verification once, " +
      s"due to releasing all native memory after the verification")
    firstRun = false
    logger.infoOrError(s"Verify assertion `${assertion.printToIR}` (Max path length: `$maxPathLength`)")
    val initialState = State(fakeInitialNode, initialValuation, indexOnPath = 0, shouldVerify = true)
    // Every CFGNode corresponds to a location, which is the location right after the node
    // fakeInitialNode corresponds to the program entry, which is right before the first command / expression
    val initialPathState = StateMap(Map(initialState.node -> initialState.valuation))
    // We compute a single abstract state for every control location
    var reached = Map[List[CFGNode], StateMap](Nil -> initialPathState)
    var waitlist: Queue[(List[CFGNode], State)] = Queue((Nil, initialState)) // Breadth First Search
    var stoppedEarly = false
    var counterexamplePaths: Set[List[CFGNode]] = Set() // Paths that lead to either refutation or timeouts
    var maximalPaths = Map[List[CFGNode], StateMap]() // Paths with a maximal length. That is, the last state is final.
    var refuted = false
    while (waitlist.nonEmpty && counterexamplePaths.isEmpty && !refuted) {
      val ((path, state), newWaitlist) = waitlist.dequeue
      logger.trace(s"Dequeued path: `$path`")
      logger.trace(s"Dequeued state: `${state.toShortString}`")
      waitlist = newWaitlist
      val oldStateMap: StateMap = reached.getOrElse(path, throw new Exception)
      val newStates = step(state)
      if (newStates.isEmpty)
        maximalPaths = maximalPaths + (path -> oldStateMap)
      newStates.foreach({
        newState =>
          logger.trace(s"New state: ${newState.toShortString}")
          val nextNode = newState.node
          val newPath = nextNode :: path
          if (newState.valuation.isBottom) {
            if (debugBottom) {
              // logger.traceOrError(s"Current node: $currentNode")
              logger.traceOrError(s"States before reaching a bottom state at node `$nextNode`:\n${oldStateMap.printStatesOnPath(path.reverse)}")
            }
          }
          val (newValuation, keepExploring) = {
            oldStateMap.valuations.get(nextNode) match {
              case Some(existingValuation) =>
                logger.trace(s"Old valuation after node `${nextNode.printToIR}`: ${existingValuation.toShortString}")
                if (existingValuation.include(newState.valuation)) {
                  logger.trace(s"The new valuation is included in the old one")
                  (existingValuation, false)
                }
                else {
                  logger.trace(s"The new valuation is not included in the old one")
                  if (debugJoinWiden) {
                    logger.traceOrError(s"Path: ${newPath.reverse.map(n => n.simplifiedString).mkString(", ")}")
                    logger.traceOrError(s"Old valuation after node `${nextNode.printToIR}`: ${existingValuation.toShortString}")
                    logger.traceOrError(s"New valuation after node `${nextNode.printToIR}`: ${newState.valuation.toShortString}")
                  }

                  val occurrences = newPath.count(node => node == nextNode)
                  val newValuation = {
                    // This number greatly affects the precision
                    if (occurrences >= arguments.getWidenThreshold) {
                      val widenedValuation = existingValuation.widen(newState.valuation)
                      if (debugJoinWiden)
                        logger.traceOrError(s"Widened valuation: `${widenedValuation.toShortString}`")
                      widenedValuation
                    } else {
                      val joinedValuation = newState.valuation.joinCopy(existingValuation)
                      if (debugJoinWiden)
                        logger.traceOrError(s"Joined valuation: `${joinedValuation.toShortString}`")
                      joinedValuation
                    }
                  }
                  if (newState.indexOnPath <= maxPathLength) (newValuation, true)
                  else {
                    logger.trace(s"Will stop exploring due to reaching the max path length `$maxPathLength` in `$newPath`")
                    stoppedEarly = true
                    // Add this path to the counterexamples, so that it can be avoided in the future
                    counterexamplePaths = counterexamplePaths + newPath
                    (newValuation, false)
                  }
                }
              case None =>
                logger.trace(s"Old valuation after node `${nextNode.printToIR}` does not exist")
                (newState.valuation, true)
            }
          }
          val newStateMap = {
            val newValuations = oldStateMap.valuations.updated(nextNode, newValuation)
            StateMap(newValuations)
          }
          reached = reached + (newPath -> newStateMap)
          if (keepExploring && !newState.valuation.isBottom) {
            waitlist = waitlist.enqueue((newPath, State(nextNode, newValuation, newState.indexOnPath, newState.shouldVerify)))
          } else {
            maximalPaths = maximalPaths + (newPath -> newStateMap)
          }

          val isNewStateRefuted =
            if (newState.shouldVerify) {
              val refuted = {
                val verified = {
                  val assertionToCheck = {
                    // Sometimes, recovering such additional information (that is lost due to widening) helps proving the assertion
                    imply(inputsArePositive, assertion)
                  }
                  if (arguments.getCheckWithZ3) newState.satisfyWithZ3(assertionToCheck)
                  else newState.satisfy(assertionToCheck)
                }
                !verified
              }
              logger.trace(s"New state ${if (refuted) "does not satisfy" else "satisfies"} assertion `$assertion`")
              if (refuted) {
                logger.infoOrError(s"Assertion violation state: `${newState.toShortString}`")
              }
              refuted
            } else false
          if (isNewStateRefuted) {
            counterexamplePaths = counterexamplePaths + newPath
            maximalPaths = maximalPaths + (newPath -> newStateMap)
            refuted = true
          }
      })
      reached = reached - path
    }
    logger.infoOrError(s"Explored `${maximalPaths.size}` maximal paths")
    val maximalPathsString = maximalPaths.map({
      case (path, stateMap) => stateMap.printStatesOnPath(path.reverse)
    }).mkString(s"\n${"-" * 150}\n")
    logger.traceOrError(s"Maximal paths:\n$maximalPathsString")
    val result = {
      val paths = counterexamplePaths.map(p => Path(p.reverse))
      if (refuted) VerifierResult(VerifierStatus.FALSE_RESULT, paths)
      else {
        if (stoppedEarly) VerifierResult(VerifierStatus.UNKNOWN_RESULT, paths)
        else VerifierResult(VerifierStatus.TRUE_RESULT, paths)
      }
    }
    val maximalPathsZ3 = {
      solver match {
        case Some(solver) =>
          maximalPaths.foldLeft(Map[List[CFGNode], StateMapBrboExpr]())({
            case (acc, (path, stateMap)) => acc + (path -> stateMap.toBrboExpr)
          })
        case None => Map[List[CFGNode], StateMapBrboExpr]()
      }
    }
    // Release all native memory occupied by apron states
    ApronMemoryManager.releaseMemory()
    Result(result, maximalPathsZ3)
  }

  // Return a set of pairs of the current node (which may be negated) and the next state
  def step(state: State): Set[State] = {
    logger.trace(s"[step] Current state: `${state.toShortString}`")
    val scopeOperations = state.valuation.scopeOperations
    val currentNode = state.node
    val nextNodes: Set[CFGNode] = {
      if (currentNode == fakeInitialNode) Set(cfg.entryNode)
      else {
        val nextNodes = cfg.findSuccessorNodes(currentNode)
        nextNodes.size match {
          case 0 | 1 | 2 =>
          case _ => throw new Exception(s"Current state `${currentNode.printToIR}` has the following successor nodes: `$nextNodes`.")
        }
        nextNodes
      }
    }
    val currentScope = scopeOperations.getScope(currentNode.command)
    logger.trace(s"[step] Current scope: `$currentScope`")
    nextNodes.map({
      nextNode =>
        logger.trace(s"[step] Next node: `${nextNode.printToIR}`")
        val nextScope = scopeOperations.getScope(nextNode.command)
        logger.trace(s"[step] Next scope: `$nextScope`")
        val valuation = {
          val valuation = {
            nextNode.command match {
              case expr: BrboExpr => evalExpr(state.valuation, expr, nextScope)
              case command: Command => evalCommand(state.valuation, command, nextScope, Some(logger))
              case _ => throw new Exception
            }
          }
          if (scopeOperations.isUnknown(currentScope) || scopeOperations.isUnknown(nextScope)
            || scopeOperations.isSubScope(nextScope, currentScope)) {
            logger.trace(s"[step] Current or next scope is unknown, or next scope is a (non-strict) sub-scope of the current scope")
            valuation
          }
          else {
            logger.trace(s"[step] Next scope is not a (non-strict) sub-scope of the current scope")
            valuation.removeVariablesInScope(currentScope)
          }
        }
        val shouldVerify = nextNode.command match {
          case _: Use => true // Only need to verify a state after executing a use command
          case _ => false
        }
        State(nextNode, valuation, state.indexOnPath + 1, shouldVerify)
    })
  }
}

object AbstractMachine {
  private val logger = MyLogger.createLogger(AbstractMachine.getClass, debugMode = false)

  /**
   *
   * @param valuation    The valuation under which the command or the expression will be evaluated
   * @param command      The command or the expression to evaluate
   * @param commandScope The lexical scope of the given command
   * @return
   */
  @tailrec
  def evalCommand(valuation: Valuation, command: Command,
                  commandScope: Scope, logger: Option[MyLogger] = None): Valuation = {
    command match {
      case VariableDeclaration(identifier, initialValue, _) =>
        assign(identifier, initialValue, createNewVariable = true, valuation, commandScope)
      case Assignment(identifier, expression, _) =>
        assign(identifier, expression, createNewVariable = false, valuation, commandScope)
      case _: CFGOnly => valuation
      case use@Use(_, update, condition, _) =>
        trace(logger, s"Evaluating `${use.printToIR}`")
        val updatedValuation =
          assign(use.resourceVariable, Addition(use.resourceVariable, update), createNewVariable = false, valuation, commandScope)
        trace(logger, s"Updated valuation: `${updatedValuation.toShortString}`")
        evalGhostCommandHelper(condition, valuation.satisfy(condition), valuation.satisfy(Negation(condition)),
          updatedValuation, valuation, logger)
      case reset@Reset(_, condition, _) =>
        trace(logger, s"Evaluating `${reset.printToIR}`")
        val counterUpdated = {
          val resourceUpdated = {
            val starUpdated = {
              /**
               * For r*, if we can decide whether r* is greater than, or less than or equal to r in the abstract state, then be precise.
               * Otherwise, we interpret as joining the post states of r* = r and r* = r*, instead of r* = r*>=r ? r* : r, because
               * (1) we only want to learn paths (as root causes), and
               * (2) this is sound and precise for the bound verification
               */
              val v = assign(reset.starVariable, reset.resourceVariable, createNewVariable = false, valuation, commandScope)
              val updateStar = valuation.satisfy(LessThan(reset.starVariable, reset.resourceVariable))
              val notUpdateStar = valuation.satisfy(greaterThanOrEqualTo(reset.starVariable, reset.resourceVariable))
              assert(!(updateStar && notUpdateStar))
              trace(logger, s"Update r*? `$updateStar`. Not update r*? `$notUpdateStar`. r*=r: ${v.toShortString}")
              trace(logger, s"r*=r*: ${valuation.toShortString}")
              if (updateStar) v
              else if (notUpdateStar) valuation
              else {
                trace(logger, s"join: ${v.joinCopy(valuation).toShortString}")
                v.joinCopy(valuation)
                v
              }
            }
            trace(logger, s"After updating r*: `${starUpdated.toShortString}`")
            assign(reset.resourceVariable, Number(0), createNewVariable = false, starUpdated, commandScope)
          }
          trace(logger, s"After updating r: `${resourceUpdated.toShortString}`")
          assign(reset.counterVariable, Addition(reset.counterVariable, Number(1)), createNewVariable = false, resourceUpdated, commandScope)
        }
        trace(logger, s"After updating r#: `${counterUpdated.toShortString}`")
        evalGhostCommandHelper(condition, valuation.satisfy(condition), valuation.satisfy(Negation(condition)),
          counterUpdated, valuation, logger)
      case Return(value, _) =>
        value match {
          case Some(_) => throw new Exception
          case None => valuation
        }
      case FunctionCallExpr(identifier, arguments, _, _) =>
        identifier match {
          case PreDefinedFunctions.NDINT3 =>
            val lower = arguments.head
            val x = arguments(1)
            val upper = arguments(2)
            val condition = And(lessThanOrEqualTo(lower, x), lessThanOrEqualTo(x, upper))
            evalExpr(valuation, condition, commandScope)
          case _ => throw new Exception
        }
      case LabeledCommand(_, command, _) => evalCommand(valuation, command, commandScope)
      case _: CexPathOnly => throw new Exception
      case Skip(_) => valuation
      case _@(Break(_) | Continue(_)) => valuation // Ignore these commands because the CFG has processed these jump commands
      case Assume(condition, _) => evalExpr(valuation, condition, commandScope)
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
    trace(logger, s"All states satisfy `${condition.printToIR}`? `$allThen`")
    trace(logger, s"All states satisfy `${Negation(condition).printToIR}`? `$allElse`")
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
                     valuation: Valuation, scopeOfNewVariable: Scope): Valuation = {
    // If x = ndInt(), then we treat x as being unconstrained
    val unconstrainVariable = {
      expr match {
        case FunctionCallExpr(identifier, _, _, _) =>
          identifier match {
            case PreDefinedFunctions.UNINITIALIZED => true
            case PreDefinedFunctions.NDINT => true
            case _ => false
          }
        case _ => false
      }
    }
    val newVariable = Variable(identifier, scopeOfNewVariable)
    val existingVariable = {
      val index = valuation.indexOfVariableThisOrParentScope(newVariable)
      if (index < 0) {
        if (createNewVariable) newVariable
        else throw new Exception(s"Cannot find variable `$identifier` within `${valuation.liveVariables}`")
      }
      else valuation.liveVariables(index)
    }
    if (unconstrainVariable) {
      if (createNewVariable) valuation.createUninitializedVariable(newVariable)
      else valuation.forgetVariable(existingVariable)
    }
    else {
      val (apronRepr, newValuation) = BrboExprUtils.toApron(expr, valuation, scopeOfNewVariable)
      val newValuation2 =
        if (createNewVariable) newValuation.createInitializedVariable(newVariable)
        else newValuation
      val variable = if (createNewVariable) newVariable else existingVariable
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
  }

  trait ToShortString {
    def toShortString: String
  }

  case class Result(result: VerifierResult, maximalPaths: Map[List[CFGNode], StateMapBrboExpr]) {
    val finalValuations: Iterable[BrboExpr] = maximalPaths.map({
      case (path, pathState) => pathState.valuations(path.head)
    })
  }

  case class State(node: CFGNode, valuation: Valuation, indexOnPath: Int, shouldVerify: Boolean) extends ToShortString {
    val scope: Scope = valuation.scopeOperations.getScope(node.command)

    def satisfy(brboExpr: BrboExpr): Boolean = valuation.satisfy(brboExpr)

    def satisfyWithZ3(brboExpr: BrboExpr): Boolean = valuation.satisfyWithZ3(brboExpr)

    def toShortString: String = s"Node: `${node.printToIR}`. Number of visits: `$indexOnPath`. shouldVerify: `$shouldVerify`. Valuation: ${valuation.toShortString}"
  }

  /**
   *
   * @param valuations A mapping from control locations to abstract states at the associated locations
   */
  case class StateMap(valuations: Map[CFGNode, Valuation]) {
    def printStatesOnPath(path: List[CFGNode]): String = {
      val pathString = path.map(n => s"  ${n.simplifiedString}").mkString("\n")
      val statesString = path.distinct.map({
        node: CFGNode =>
          val nodeString = node.simplifiedString
          val stateString = valuations(node).toShortString
          StringFormatUtils.wrapColumnsFixedWidth(List((nodeString, 35), (stateString, 150)))
      }).mkString("\n")
      s"Path (${path.length}):\n$pathString\nStates (${valuations.size}):\n$statesString"
    }

    def toBrboExpr: StateMapBrboExpr = {
      val valuationsBrboExpr = valuations.foldLeft(Map[CFGNode, BrboExpr]())({
        case (acc, (node, valuation)) =>
          acc + (node -> valuation.stateToBrboExpr)
      })
      StateMapBrboExpr(valuationsBrboExpr)
    }
  }

  case class StateMapBrboExpr(valuations: Map[CFGNode, BrboExpr]) extends Serializable

  /**
   *
   * @param liveVariables   All variables that can be looked up or re-assigned in the current program state
   * @param apronState      The abstraction of the current program state
   * @param allVariables    All variables that have been declared in apronState
   * @param scopeOperations Functions to manipulate scopes
   * @param logger          The logger
   */
  case class Valuation(liveVariables: List[Variable],
                       apronState: Abstract0,
                       allVariables: List[Variable],
                       scopeOperations: ScopeOperations,
                       logger: Option[MyLogger] = None) extends ToShortString { // TODO: Let this logger be non-optional
    assert(apronState != null)
    private val manager = apronState.getCreationManager
    // traceOrError(logger, s"Create a new state. isTop: `${apronState.isTop(manager)}`. isBottom: `${apronState.isBottom(manager)}`")
    private val debugMode = logger match {
      case Some(logger) => logger.debugMode
      case None => false
    }
    val variablesNoScope: List[Identifier] = liveVariables.map(v => v.identifier)
    val allVariablesNoScope: List[Identifier] = allVariables.map(v => v.identifier)
    private val allVariablesNames: Array[String] = allVariablesNoScope.zipWithIndex
      .map({ case (identifier, index) => s"${identifier.name}(${StringFormatUtils.integer(index, 2)})" }).toArray
    private val allVariablesShortNames: Array[String] = allVariablesNoScope.zipWithIndex
      .map({ case (identifier, _) => s"${identifier.name}" }).toArray
    val allVariablesSet: Set[Variable] = allVariables.toSet
    private val solver = new Z3Solver
    allVariables.foreach(v => solver.mkIntVar(v.identifier.name))

    private val constraints: Array[BrboExpr] = apronState.toTcons(manager).map(t => Apron.constraintToBrboExpr(t, allVariablesNoScope))
    private val stateAst = stateToZ3Ast(solver)
    private val stateString = apronState.toString(manager, allVariablesNames)
    private val stateShortString = apronState.toString(manager, allVariablesShortNames)

    def indexOfVariableThisScope(variable: Variable): Int =
      liveVariables.indexWhere(v => v.identifier.sameAs(variable.identifier) && scopeOperations.isSame(v.scope, variable.scope))

    @tailrec
    final def indexOfVariableThisOrParentScope(variable: Variable): Int = {
      val index = indexOfVariableThisScope(variable)
      if (index != -1) index
      else {
        variable.scope match {
          case Some(scope) => indexOfVariableThisOrParentScope(Variable(variable.identifier, scopeOperations.getScope(scope)))
          case None => -1
        }
      }
    }

    def apronVariableThisScope(variable: Variable): Texpr0Node = Apron.mkVar(indexOfVariableThisScope(variable))

    def apronVariableAnyScope(variable: Variable): Texpr0Node = Apron.mkVar(indexOfVariableThisOrParentScope(variable))

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
      Valuation(liveVariables :+ variable, newApronState, allVariables :+ variable, scopeOperations, logger)
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

    def include(other: Valuation): Boolean = {
      try {
        other.apronState.isIncluded(manager, apronState)
      }
      catch {
        case _: Exception =>
          // May be caused by declaring new variables along the execution
          false
      }
    }

    def joinCopy(other: Valuation): Valuation = {
      val (v1: Valuation, v2: Valuation) = equalizeVariables(other)
      val newApronState = v1.apronState.joinCopy(manager, v2.apronState)
      Valuation(v1.liveVariables, newApronState, v1.allVariables, scopeOperations, logger)
    }

    def widen(other: Valuation): Valuation = {
      val (v1: Valuation, v2: Valuation) = equalizeVariables(other)
      val newApronState = v1.apronState.widening(manager, v2.apronState)
      Valuation(v1.liveVariables, newApronState, v1.allVariables, scopeOperations, logger)
    }

    private def equalizeVariables(other: Valuation): (Valuation, Valuation) = {
      if (other.allVariablesSet.subsetOf(allVariablesSet)) {
        (this, createUninitializedVariables(other, this))
      }
      else if (allVariablesSet.subsetOf(other.allVariablesSet)) {
        (createUninitializedVariables(this, other), other)
      }
      else throw new Exception
    }

    /**
     *
     * @param less The valuation whose declared variables in the Apron state is a subset of the other valuation
     * @param more The other valuation
     * @return A valuation that is created from `less` but contains the same variables as `more`
     */
    private def createUninitializedVariables(less: Valuation, more: Valuation): Valuation = {
      val v1 = more.allVariables.foldLeft(less)({
        (acc, v) =>
          if (less.allVariables.contains(v)) acc
          else acc.createUninitializedVariable(v)
      })
      // The newly created variables in v1 are not necessarily live variables
      val v2 = Valuation(more.liveVariables, v1.apronState, v1.allVariables, v1.scopeOperations, v1.logger)
      assert(v1.allVariables == more.allVariables,
        s"v.allVariables: ${v1.allVariablesNoScope}\nmore.allVariables: ${more.allVariablesNoScope}\nless.allVariables:${less.allVariablesNoScope}")
      v2
    }

    def assignCopy(variable: Variable, expression: Texpr0Node): Valuation = {
      val index = indexOfVariableThisOrParentScope(variable)
      if (index == -1) this
      else {
        val newState = apronState.assignCopy(manager, index, new Texpr0Intern(expression), null)
        Valuation(liveVariables, newState, allVariables, scopeOperations, logger)
      }
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
    }

    def satisfyWithZ3(constraint: Constraint): Boolean = {
      constraint match {
        case Apron.Conjunction(left, right) => satisfyWithZ3(left) && satisfyWithZ3(right)
        case Apron.Disjunction(left, right) => satisfyWithZ3(left) || satisfyWithZ3(right)
        case Singleton(constraint) => satisfyWithZ3(constraint)
        case _ => throw new Exception
      }
    }

    def satisfyWithZ3(constraint: Tcons0): Boolean = {
      val constraintZ3 = Apron.constraintToZ3(constraint, solver, allVariablesNoScope)
      satisfyWithZ3(constraintZ3)
    }

    def satisfyWithZ3(constraint: BrboExpr): Boolean = {
      val constraintZ3 = constraint.toZ3AST(solver)
      satisfyWithZ3(constraintZ3)
    }

    private def satisfyWithZ3(ast: AST): Boolean = {
      val imply = solver.mkImplies(solver.mkAnd(stateAst), ast)
      // error(logger, s"[check valuation satisfiability] imply: $imply")
      solver.checkAssertionForallPushPop(imply, debugMode)
    }

    def imposeConstraint(constraint: Constraint): Valuation = {
      val newStates = Apron.imposeConstraint(apronState, constraint)
      if (newStates.size > 1)
        trace(logger, s"Approximate the disjunction in `$constraint` with a conjunctive domain")
      val joinedState = newStates.head
      newStates.tail.foreach({
        newState => joinedState.join(manager, newState)
      })
      // The head new state will be registered when creating the valuation
      Valuation(liveVariables, joinedState, allVariables, scopeOperations, logger)
    }

    def toBottom: Valuation = imposeConstraint(Singleton(Apron.mkEqZero(Apron.mkIntVal(1))))

    def isBottom: Boolean = apronState.isBottom(manager)

    def removeVariablesInScope(scope: Scope): Valuation = {
      val firstIndex = liveVariables.indexWhere(v => scopeOperations.isSame(v.scope, scope))
      if (firstIndex == -1) this
      else {
        val (toKeep, toRemove) = liveVariables.splitAt(firstIndex)
        trace(logger, s"Removing variables in scope `$scope` from variables `$liveVariables`")
        trace(logger, s"toRemove: `$toRemove`")
        assert(toRemove.forall(v => v.scope == scope))
        Valuation(toKeep, apronState, allVariables, scopeOperations, logger)
      }
    }

    def forgetVariable(variable: Variable): Valuation = {
      val index = allVariables.indexWhere(v => v.identifier.sameAs(variable.identifier) && scopeOperations.isSame(v.scope, variable.scope))
      if (index >= 0 && index <= allVariables.length) {
        val newState = apronState.forgetCopy(manager, index, false)
        Valuation(liveVariables, newState, allVariables, scopeOperations, logger)
      }
      else {
        error(logger, s"Variable `$variable` cannot be found in `$allVariables`")
        this
      }
    }

    override def toString: String = {
      val variablesString = "  " + liveVariables.map(v => v.toString).mkString("\n  ")
      s"Variables:\n$variablesString\nApronState: $stateString"
    }

    override def toShortString: String = {
      // val variablesString = liveVariables.map(v => v.toShortString)
      // String.format("Variables: %-30s ApronState: %-50s", variablesString, stateString)
      s"ApronState: $stateShortString"
    }

    def stateToZ3Ast(solver: Z3Solver): AST = {
      val asts = constraints.map(c => c.toZ3AST(solver))
      if (asts.nonEmpty) solver.mkAnd(asts: _*)
      else solver.mkTrue()
    }

    def stateToBrboExpr: BrboExpr = {
      constraints.foldLeft(Bool(b = true): BrboExpr)({
        (acc, constraint) => And(acc, constraint)
      })
    }
  }

  case class Variable(identifier: Identifier, scope: Scope) extends ToShortString {
    override def toShortString: String = identifier.toString
  }

  type Scope = Option[Statement]
  val TOP_SCOPE: Scope = Some(Block(Nil))
  val UNKNOWN_SCOPE: Scope = None
  val DEFAULT_SCOPE_OPERATIONS = new ScopeOperations(Map())

  class ScopeOperations(parentStatements: Map[BrboAst, Statement]) {
    def getScope(node: BrboAst): Scope = parentStatements.get(node)

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
    Valuation(Nil, state, Nil, scopeOperations, logger)
  }

  private def traceOrError(logger: Option[MyLogger], message: String): Unit = {
    logger match {
      case Some(logger) => logger.traceOrError(message)
      case None =>
    }
  }

  private def trace(logger: Option[MyLogger], message: String): Unit = {
    logger match {
      case Some(logger) => logger.trace(message)
      case None =>
    }
  }

  private def error(logger: Option[MyLogger], message: String): Unit = {
    logger match {
      case Some(logger) => logger.error(message)
      case None =>
    }
  }

  def heapSize: String = StringFormatUtils.formatSize(Runtime.getRuntime.totalMemory)
}
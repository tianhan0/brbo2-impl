package brbo.backend.refiner

import brbo.backend.verifier.{AbstractInterpreter, InterpreterKind, SymbolicExecution}
import brbo.common._
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import com.microsoft.z3.AST

class Synthesizer(originalProgram: BrboProgram, argument: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[Synthesizer], argument.getDebugMode)
  private val allCommands = BrboAstUtils.collectCommands(originalProgram.mainFunction.actualBody)
  private val useCommands = allCommands.filter(command => command.isInstanceOf[Use])
  private val resetCommands = allCommands.filter(command => command.isInstanceOf[Reset])

  private val inputVariables = originalProgram.mainFunction.parameters
  private val solver = new Z3Solver
  private val predicates: List[Predicate] = {
    val allNonGhostVariables = {
      val allVariables = originalProgram.mainFunction.parameters.toSet ++
        BrboAstUtils.collectUseDefVariables(originalProgram.mainFunction.bodyNoInitialization)
      allVariables.filter(v => !GhostVariableUtils.isGhostVariable(v.name))
    }
    val allPredicates = Predicate.generatePredicates(allNonGhostVariables, argument.getRelationalPredicates)
    val filterFalsePredicates = allPredicates.filter({
      p =>
        val isFalse = solver.checkAssertionForallPushPop(solver.mkIff(p.toAst(solver), solver.mkFalse()))
        if (isFalse) logger.traceOrError(s"Predicate `${p.expr.prettyPrintToCFG}` is false!")
        !isFalse
    })
    logger.traceOrError(s"Candidate predicates (`${filterFalsePredicates.size}`): `$filterFalsePredicates`")
    filterFalsePredicates
  }

  def synthesize(refinement: Refinement): BrboProgram = {
    logger.infoOrError(s"Synthesize a new main function from a refinement: `${refinement.toStringNoPath}`.")
    val useReplacements: Map[Command, Set[Command]] = useCommands.foldLeft(Map[Command, Set[Command]]())({
      (useMap, useCommand) =>
        val use = useCommand.asInstanceOf[Use]
        val indexMap = refinement.getSplitUseInstances(use)
        val newUses: Set[Command] = {
          if (indexMap.nonEmpty) {
            logger.traceOrError(s"Split-Use: Synthesize new uses from old use `${use.prettyPrintToCFG}`")
            computeNewUses(refinement.path, use, indexMap)
          }
          else {
            logger.traceOrError(s"Split-Use: Old use `${use.prettyPrintToCFG}` is not split")
            Set(use)
          }
        }
        logger.traceOrError(s"Split-Use: Old use `${use.prettyPrintToCFG}` is replaced by `$newUses`")
        useMap + (use -> newUses)
    })

    val resetReplacements: Map[Command, Set[Command]] = resetCommands.foldLeft(Map[Command, Set[Command]]())({
      (acc, resetCommand) =>
        val reset = resetCommand.asInstanceOf[Reset]
        logger.traceOrError(s"Synthesize-Reset: Synthesize new reset(s) from old reset: `${reset.prettyPrintToCFG}`")
        val indexMap = refinement.getResetInstances(reset)
        val newResets: Set[Command] = indexMap.foldLeft(Set[Command]())({
          case (acc, (newGroupId, (keepSet, removeSet))) =>
            logger.traceOrError(s"Synthesize-Reset: New group `$newGroupId` (which replaces old group: `${reset.groupId})")
            val newReset = computeNewReset(refinement.path, newGroupId, keepSet, removeSet, reset.condition)
            acc + newReset
        })
        logger.traceOrError(s"Synthesize-Reset: Old reset `${reset.prettyPrintToCFG}` is replaced by `$newResets`")
        acc + (reset -> newResets)
    })

    val newMainBody = (useReplacements ++ resetReplacements).foldLeft(originalProgram.mainFunction.bodyNoInitialization: BrboAst)({
      case (acc, (command, newCommands)) =>
        val commandsInList = newCommands.toList.sortWith({ case (c1, c2) => c1.prettyPrintToC() < c2.prettyPrintToC() })
        BrboAstUtils.replaceAst(acc, command, Block(commandsInList))
    })
    logger.infoOrError(s"Successful: New main function body:\n`$newMainBody`")

    val newGroupIds: Set[Int] = BrboAstUtils.collectCommands(newMainBody).flatMap({
      command =>
        command match {
          case Reset(groupId, _, _) => Some(groupId)
          case Use(groupId, _, _, _) => groupId
          case _ => None
        }
    })
    logger.trace(s"Old groups: `${originalProgram.mainFunction.groupIds}`")
    logger.trace(s"New groups from the path refinement: `${refinement.groupIds}`")
    logger.trace(s"New groups (overall): `$newGroupIds`")
    val newMainFunction = originalProgram.mainFunction
      .replaceBodyWithoutInitialization(newMainBody.asInstanceOf[Statement])
      .replaceGroupIds(newGroupIds)
    originalProgram.replaceMainFunction(newMainFunction)
  }

  def computeNewUses(path: List[CFGNode], oldUse: Use, indexMap: Map[Int, Set[Int]]): Set[Command] = {
    val candidateGuardMap = indexMap.foldLeft(Map[Int, List[Predicate]]())({
      case (acc, (groupId, indices)) =>
        val postCondition = computePostCondition(path, indices)
        logger.traceOrError(s"Split-Use: Compute predicates implied from post conditions right before use instances in new group `$groupId`")
        val impliedPredicates = computeImpliedPredicates(postCondition)
        acc + (groupId -> impliedPredicates)
    })

    val sortedIds = candidateGuardMap.keys.toList.sorted
    logger.traceOrError(s"Split-Use: Compute candidate predicates for partitioning old group `${oldUse.groupId.get}`")
    val candidatePredicates = computeCandidatePredicates(
      sortedIds.map(id => candidateGuardMap.getOrElse(id, throw new Exception)), oldUse.condition)

    sortedIds.zip(candidatePredicates.head).map({
      case (groupId, predicate) =>
        val finalPredicate = And(predicate.expr, oldUse.condition)
        logger.traceOrError(s"Split-Use: Choose predicate `$finalPredicate` for new group `$groupId` (partitioned from old group `${oldUse.groupId.get})`")
        Use(Some(groupId), oldUse.update.uniqueCopyExpr, finalPredicate).asInstanceOf[Command]
    }).toSet
  }

  def computeNewReset(path: List[CFGNode], groupIdForNewReset: Int, keepSet: Set[Int], removeSet: Set[Int], guardOfReset: BrboExpr): Reset = {
    // Necessary short circuit. Otherwise we will compute post-conditions for an empty trace, whose semantics is unclear.
    if (removeSet.isEmpty)
      return Reset(groupIdForNewReset, guardOfReset.uniqueCopyExpr)
    if (keepSet.isEmpty)
      return Reset(groupIdForNewReset, Bool(b = false))
    val postConditionKeep = computePostCondition(path, keepSet)
    logger.traceOrError(s"Synthesize-Reset: Compute predicates implied from post conditions right before keeping resets in group `$groupIdForNewReset`")
    val impliedPredicatesKeep = computeImpliedPredicates(postConditionKeep)

    val postConditionRemove = computePostCondition(path, removeSet)
    logger.traceOrError(s"Synthesize-Reset: Compute predicates implied from post conditions right before removing resets in group `$groupIdForNewReset`")
    val impliedPredicatesRemove = computeImpliedPredicates(postConditionRemove)

    logger.traceOrError(s"Synthesize-Reset: Compute candidate predicates for partitioning resets from group `$groupIdForNewReset`")
    val candidatePredicates = computeCandidatePredicates(List(impliedPredicatesKeep, impliedPredicatesRemove), guardOfReset)
    val (predicateKeep, _) = (candidatePredicates.head.head, candidatePredicates.head.last)
    Reset(groupIdForNewReset, And(guardOfReset.uniqueCopyExpr, predicateKeep.expr)) // Only keep the reset under the computed predicate
  }

  def computePostCondition(path: List[CFGNode], indices: Set[Int]): AST = {
    assert(indices.nonEmpty)
    val postConditions = indices.map({
      index =>
        // TODO: It is probably fine to use the model checker, because
        //  - we only use linear constraints to partition the states, and
        //  - if linear constraints can partition the concrete states, then they may also partition the abstract states
        val (ast, _) =
          AbstractInterpreter.interpretPath(path.slice(0, index), inputVariables, solver, InterpreterKind.MODEL_CHECK, argument)
        ast
    }).toSeq
    solver.mkOr(postConditions: _*)
  }

  def computeImpliedPredicates(whatToImplyFrom: AST): List[Predicate] = {
    val query = solver.mkIff(whatToImplyFrom, solver.mkFalse())
    val isFalse = solver.checkAssertionForallPushPop(query)
    assert(!isFalse, s"$query")
    // Return false (instead of all predicates) when whatToImplyFrom is false
    // if (isFalse) return List(Predicate(Bool(b = false)))
    val result = predicates.filter({
      predicate =>
        val imply = solver.mkImplies(whatToImplyFrom, predicate.toAst(solver))
        solver.checkAssertionForallPushPop(imply)
    })
    logger.traceOrError(s"`${result.size}` predicates are implied:\n`$result`")
    result
  }

  def computeCandidatePredicates(predicates: Iterable[Iterable[Predicate]], notConflictWith: BrboExpr): List[List[Predicate]] = {
    val candidatePredicates = MathUtils.crossJoin(predicates).filter({
      predicates =>
        val isPartitionResult = Synthesizer.isDisjoint(predicates, solver)
        val isCoverResult = Synthesizer.isCover(predicates, solver)
        val notConflictWithResult = Synthesizer.notConflictWith(predicates, notConflictWith, solver)
        // logger.traceOrError(s"Predicates `$predicates` ${if (isPartitionResult) "are" else "are not"} a partition, and ${if (isCoverResult) "are" else "are not"} a cover.")
        isPartitionResult && isCoverResult && notConflictWithResult
    })
    if (candidatePredicates.isEmpty) {
      logger.traceOrError(s"No predicate can distinguish the partitioned instances")
      throw new SynthesisFailException
    }
    logger.traceOrError(s"Predicates that can partition instances:\n`${candidatePredicates.mkString("\n")}`")
    candidatePredicates
  }
}

object Synthesizer {
  private val logger = MyLogger.createLogger(Synthesizer.getClass, debugMode = false)

  // Any two predicate should be disjoint with each other
  def isDisjoint(predicates: Iterable[Predicate], solver: Z3Solver): Boolean = {
    MathUtils.choose2(predicates).forall({
      case (p1, p2) =>
        val disjoint = solver.checkAssertionForallPushPop(solver.mkIff(solver.mkAnd(p1.toAst(solver), p2.toAst(solver)), solver.mkFalse()))
        logger.traceOrError(s"Decide the disjointness between `${p1.expr.prettyPrintToCFG}` and `${p2.expr.prettyPrintToCFG}`: `$disjoint`")
        disjoint
    })
  }

  // predicates are created with solver
  def isCover(predicates: Iterable[Predicate], solver: Z3Solver): Boolean = {
    solver.checkAssertionForallPushPop(
      solver.mkIff(solver.mkOr(predicates.toSeq.map(p => p.toAst(solver)): _*), solver.mkTrue()))
  }

  def notConflictWith(predicates: Iterable[Predicate], target: BrboExpr, solver: Z3Solver): Boolean = {
    predicates.forall({
      p => solver.checkAssertionPushPop(solver.mkAnd(p.toAst(solver), target.toZ3AST(solver)))
    })
  }
}

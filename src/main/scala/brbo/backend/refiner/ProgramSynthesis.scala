package brbo.backend.refiner

import brbo.backend.verifier.SymbolicExecution
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{CommandLineArguments, MathUtils, MyLogger}
import com.microsoft.z3.AST

class ProgramSynthesis(brboProgram: BrboProgram, commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[ProgramSynthesis], commandLineArguments.getDebugMode)
  private val allCommands = BrboAstUtils.collectCommands(brboProgram.mainFunction.actualBody)
  private val useCommands = allCommands.filter(command => command.isInstanceOf[Use])
  private val resetCommands = allCommands.filter(command => command.isInstanceOf[Reset])

  private val symbolicExecution = new SymbolicExecution(brboProgram.mainFunction.parameters)
  private val predicates: List[Predicate] = {
    val allVariables = brboProgram.mainFunction.parameters.toSet ++ BrboAstUtils.collectUseDefVariables(brboProgram.mainFunction.bodyWithoutInitialization)
    val result = Predicate.generatePredicates(allVariables, symbolicExecution.solver)
    logger.traceOrError(s"Candidate predicates (`${result.size}`): `$result`")
    result
  }

  def synthesize(refinement: Refinement): BrboProgram = {
    logger.info(s"Synthesize a new main function from a refinement: `${refinement.toStringNoPath}`.")
    val useReplacements: Map[Command, Set[Command]] = useCommands.foldLeft(Map[Command, Set[Command]]())({
      (useMap, useCommand) =>
        val use = useCommand.asInstanceOf[Use]
        logger.traceOrError(s"Synthesize new uses from old use: `${use.prettyPrintToCFG}`")
        val indexMap = refinement.getSplitUseInstances(use)
        val newUses = computeNewUses(refinement.path, use, indexMap)
        useMap + (use -> newUses)
    })

    val resetReplacements: Map[Command, Set[Command]] = resetCommands.foldLeft(Map[Command, Set[Command]]())({
      (acc, resetCommand) =>
        val reset = resetCommand.asInstanceOf[Reset]
        logger.traceOrError(s"Synthesize new reset(s) from old reset: `${reset.prettyPrintToCFG}`")
        val indexMap = refinement.getResetInstances(reset)
        val newResets: Set[Command] = indexMap.size match {
          case 0 => Set()
          case 1 =>
            logger.traceOrError(s"Remove resets for old group `${reset.groupId}")
            val newReset = computeNewReset(refinement.path, reset.groupId, indexMap.head._2._1, indexMap.head._2._2, reset.condition)
            logger.traceOrError(s"New reset: `$newReset`")
            Set(newReset)
          case _ =>
            val result = indexMap.foldLeft(Set[Command]())({
              case (acc, (newGroupId, (keepSet, removeSet))) =>
                logger.traceOrError(s"Remove resets for new group `$newGroupId` (which replaces old group: `${reset.groupId})")
                val newReset = computeNewReset(refinement.path, newGroupId, keepSet, removeSet, reset.condition)
                acc + newReset
            })
            logger.traceOrError(s"New resets: `$result`")
            result
        }
        acc + (reset -> newResets)
    })

    val newMainBody = (useReplacements ++ resetReplacements).foldLeft(brboProgram.mainFunction.bodyWithoutInitialization: BrboAst)({
      case (acc, (command, newCommands)) =>
        val commandsInList = newCommands.toList.sortWith({ case (c1, c2) => c1.prettyPrintToC() < c2.prettyPrintToC() })
        BrboAstUtils.replace(acc, command, Block(commandsInList))
    })
    logger.infoOrError(s"[Synthesis successful] New main function body:\n`$newMainBody`")

    val newGroupIds: Set[Int] = brboProgram.mainFunction.groupIds -- refinement.groupIds.keySet ++ refinement.groupIds.values.flatten
    logger.infoOrError(s"[Synthesis successful] New groups: `$newGroupIds`")
    val newMainFunction =
      brboProgram.mainFunction
        .replaceBodyWithoutInitialization(newMainBody.asInstanceOf[Statement])
        .replaceGroupIds(newGroupIds)
    brboProgram.replaceMainFunction(newMainFunction)
  }

  def computeNewUses(path: List[CFGNode], oldUse: Use, indexMap: Map[Int, Set[Int]]): Set[Command] = {
    val candidateGuardMap = indexMap.foldLeft(Map[Int, List[Predicate]]())({
      case (acc, (groupId, indices)) =>
        val postCondition = computePostCondition(path, indices)
        logger.traceOrError(s"Compute predicates implied from post conditions right before use instances in new group `$groupId`")
        val impliedPredicates = computeImpliedPredicates(postCondition, oldUse.condition)
        acc + (groupId -> impliedPredicates)
    })

    val sortedIds = candidateGuardMap.keys.toList.sorted
    logger.traceOrError(s"Compute candidate predicates for partitioning old group `${oldUse.groupId.get}")
    val candidatePredicates = computeCandidatePredicates(
      sortedIds.map(id => candidateGuardMap.getOrElse(id, throw new Exception)))

    sortedIds.zip(candidatePredicates.head).map({
      case (groupId, predicate) =>
        val finalPredicate = And(predicate.expr, oldUse.condition)
        logger.traceOrError(s"Split Use: Choose predicate `$finalPredicate` for new group `$groupId` (partitioned from old group `${oldUse.groupId.get})`")
        Use(Some(groupId), oldUse.update, finalPredicate).asInstanceOf[Command]
    }).toSet
  }

  def computeNewReset(path: List[CFGNode], groupIdForNewReset: Int, keepSet: Set[Int], removeSet: Set[Int], guardOfReset: BrboExpr): Reset = {
    if (removeSet.isEmpty)
      return Reset(groupIdForNewReset, guardOfReset) // Short circuit
    val postConditionKeep = computePostCondition(path, keepSet)
    logger.traceOrError(s"Compute predicates implied from post conditions right before keeping resets in group `$groupIdForNewReset`")
    val impliedPredicatesKeep = computeImpliedPredicates(postConditionKeep, guardOfReset)

    val postConditionRemove = computePostCondition(path, removeSet)
    logger.traceOrError(s"Compute predicates implied from post conditions right before removing resets in group `$groupIdForNewReset`")
    val impliedPredicatesRemove = computeImpliedPredicates(postConditionRemove, guardOfReset)

    logger.traceOrError(s"Compute candidate predicates for partitioning resets from group `$groupIdForNewReset")
    val candidatePredicates = computeCandidatePredicates(List(impliedPredicatesKeep, impliedPredicatesRemove))
    val (predicateKeep, _) = (candidatePredicates.head.head, candidatePredicates.head.last)
    Reset(groupIdForNewReset, And(guardOfReset, predicateKeep.expr)) // Only keep the reset under the computed predicate
  }

  def computePostCondition(path: List[CFGNode], indices: Set[Int]): AST = {
    val postConditions = indices.map({
      index =>
        val valuation = symbolicExecution.execute(path.slice(0, index)).valuations.head
        SymbolicExecution.valuationToAST(valuation, symbolicExecution.solver)
    }).toSeq
    symbolicExecution.solver.mkOr(postConditions: _*)
  }

  def computeImpliedPredicates(whatToImplyFrom: AST, excludeFromResult: BrboExpr): List[Predicate] = {
    val solver = symbolicExecution.solver
    val result = predicates.filter({
      predicate =>
        val query = solver.mkImplies(whatToImplyFrom, solver.mkAnd(predicate.ast, excludeFromResult.toZ3AST(solver)))
        solver.checkAssertionPushPop(query, printUnsatCore = false)
    })
    logger.traceOrError(s"These predicates are implied (excluding `${excludeFromResult.prettyPrintToCFG}`):\n`$result`")
    result
  }

  def computeCandidatePredicates(predicates: Iterable[Iterable[Predicate]]): List[List[Predicate]] = {
    val candidatePredicates = MathUtils.crossJoin(predicates).filter({
      predicates =>
        val isPartitionResult = isPartition(predicates)
        val isCoverResult = isCover(predicates)
        logger.traceOrError(s"Predicates `$predicates` ${if (isPartitionResult) "are" else "are not"} a partition, " +
          s"and ${if (isCoverResult) "are" else "are not"} a cover.")
        isPartitionResult && isCoverResult
    })
    if (candidatePredicates.isEmpty) {
      logger.traceOrError(s"No predicate can distinguish the partitioned instances`")
      throw new SynthesisFailException
    }
    candidatePredicates
  }

  def isPartition(predicates: Iterable[Predicate]): Boolean = {
    val solver = symbolicExecution.solver
    MathUtils.choose2(predicates).forall({
      case (p1, p2) =>
        // Any two predicate should conflict with each other
        !solver.checkAssertionPushPop(solver.mkAnd(p1.ast, p2.ast), printUnsatCore = false)
    })
  }

  def isCover(predicates: Iterable[Predicate]): Boolean = {
    val solver = symbolicExecution.solver
    solver.checkAssertionPushPop(
      solver.mkIff(solver.mkOr(predicates.toSeq.map(p => p.ast): _*), solver.mkTrue()), printUnsatCore = false)
  }
}

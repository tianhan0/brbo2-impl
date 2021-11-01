package brbo.backend.refiner

import brbo.backend.verifier.SymbolicExecution
import brbo.common.ast._
import brbo.common.{CommandLineArguments, MathUtils, MyLogger, Z3Solver}
import com.microsoft.z3.AST

class ProgramSynthesis(commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[ProgramSynthesis], commandLineArguments.getDebugMode)

  def synthesize(brboProgram: BrboProgram, refinement: Refinement, symbolicExecution: SymbolicExecution): BrboProgram = {
    logger.info(s"Synthesize a new main function from a refinement: `${refinement.toStringNoPath}`.")
    val allCommands = BrboAstUtils.collectCommands(brboProgram.mainFunction.body)
    val useCommands = allCommands.filter(command => command.isInstanceOf[Use])
    val resetCommands = allCommands.filter(command => command.isInstanceOf[Reset])

    val solver = symbolicExecution.solver
    val predicates: List[Predicate] = {
      val allVariables = brboProgram.mainFunction.parameters.toSet ++ BrboAstUtils.collectUseDefVariables(brboProgram.mainFunction.body)
      Predicate.generatePredicates(allVariables, solver)
    }
    logger.traceOrError(s"Candidate predicates: `$predicates`")

    def computePostCondition(indices: Set[Int]): AST = {
      val postConditions = indices.map({
        index =>
          val valuation = symbolicExecution.execute(refinement.path.slice(0, index)).valuations.head
          SymbolicExecution.valuationToAST(valuation, solver)
      }).toSeq
      solver.mkOr(postConditions: _*)
    }

    def computeNewReset(groupIdForNewReset: Int, keepSet: Set[Int], removeSet: Set[Int], guardOfReset: BrboExpr): Reset = {
      if (removeSet.isEmpty)
        return Reset(groupIdForNewReset, guardOfReset) // Short circuit
      val postConditionKeep = computePostCondition(keepSet)
      val impliedPredicatesKeep = computeImpliedPredicates(predicates, postConditionKeep, guardOfReset, solver)
      val postConditionRemove = computePostCondition(removeSet)
      val impliedPredicatesRemove = computeImpliedPredicates(predicates, postConditionRemove, guardOfReset, solver)
      logger.traceOrError(s"These predicates may distinguish keeping or removing reset instances in group `$groupIdForNewReset`: `$impliedPredicatesRemove`")
      logger.traceOrError(s"Compute candidate predicates for partitioning resets from group `$groupIdForNewReset")
      val candidatePredicates = computeCandidatePredicates(List(impliedPredicatesKeep, impliedPredicatesRemove), solver)
      Reset(groupIdForNewReset, And(guardOfReset, candidatePredicates.head.head.expr)) // Only keep the reset under the computed predicate
    }

    val useReplacements: Map[Command, Set[Command]] =
      useCommands.foldLeft(Map[Command, Set[Command]]())({
        (useMap, useCommand) =>
          val use = useCommand.asInstanceOf[Use]
          val indexMap = refinement.getSplitUseInstances(use)
          val candidateGuardMap = indexMap.foldLeft(Map[Int, List[Predicate]]())({
            case (acc, (groupId, indices)) =>
              val postCondition = computePostCondition(indices)
              val impliedPredicates = computeImpliedPredicates(predicates, postCondition, use.condition, solver)
              logger.traceOrError(s"These predicates may distinguish use instances in new group `$groupId`: `$impliedPredicates`")
              acc + (groupId -> impliedPredicates)
          })

          val sortedIds = candidateGuardMap.keys.toList.sorted
          logger.traceOrError(s"Compute candidate predicates for partitioning old group `${use.groupId.get}")
          val candidatePredicates = computeCandidatePredicates(
            sortedIds.map(id => candidateGuardMap.getOrElse(id, throw new Exception)), solver)

          val newUses = sortedIds.zip(candidatePredicates.head).map({
            case (groupId, predicate) =>
              val finalPredicate = And(predicate.expr, use.condition)
              logger.traceOrError(s"Split Use: Choose predicate `$finalPredicate` for new group `$groupId` (partitioned from old group `${use.groupId.get})`")
              Use(Some(groupId), use.update, finalPredicate).asInstanceOf[Command]
          }).toSet

          useMap + (use -> newUses)
      })

    val resetReplacements = resetCommands.foldLeft(Map[Command, Set[Command]]())({
      (acc, resetCommand) =>
        val reset = resetCommand.asInstanceOf[Reset]
        val indexMap = refinement.getResetInstances2(reset)
        val newResets: Set[Command] = indexMap.size match {
          case 0 => Set()
          case 1 =>
            logger.traceOrError(s"Remove resets for old group `${reset.groupId}")
            val newReset = computeNewReset(reset.groupId, indexMap.head._2._1, indexMap.head._2._2, reset.condition)
            Set(newReset)
          case _ =>
            indexMap.foldLeft(Set[Command]())({
              case (acc, (newGroupId, (keepSet, removeSet))) =>
                logger.traceOrError(s"Remove resets for new group `$newGroupId` (which replaces old group: `${reset.groupId})")
                val newReset = computeNewReset(newGroupId, keepSet, removeSet, reset.condition)
                acc + newReset
            })
        }
        acc + (reset -> newResets)
    })

    val newMainBody = (useReplacements ++ resetReplacements).foldLeft(brboProgram.mainFunction.body: BrboAst)({
      case (acc, (command, newCommands)) =>
        val commandsInList = newCommands.toList.sortWith({ case (c1, c2) => c1.prettyPrintToC() < c2.prettyPrintToC() })
        BrboAstUtils.replace(acc, command, Block(commandsInList))
    })
    logger.infoOrError(s"[Synthesis successful] New main function body:\n`$newMainBody`")

    val newGroupIds: Set[Int] = brboProgram.groupIds -- refinement.groupIDs.keySet ++ refinement.groupIDs.values.flatten
    logger.infoOrError(s"[Synthesis successful] New groups: `$newGroupIds`")
    val newMainFunction = brboProgram.mainFunction.replaceBody(newMainBody.asInstanceOf[Statement])
    brboProgram.replaceMainFunction(newMainFunction, newGroupIds)
  }

  private def computeImpliedPredicates(predicates: List[Predicate], whatToImplyFrom: AST,
                                       excludeFromResult: BrboExpr, solver: Z3Solver): List[Predicate] = {
    predicates.filter({
      predicate =>
        val query = solver.mkImplies(whatToImplyFrom, solver.mkAnd(predicate.ast, excludeFromResult.toZ3AST(solver)))
        solver.checkAssertionPushPop(query, printUnsatCore = false)
    })
  }

  private def computeCandidatePredicates(predicates: Iterable[Iterable[Predicate]], solver: Z3Solver): List[List[Predicate]] = {
    val candidatePredicates = MathUtils.crossJoin(predicates).filter({
      predicates =>
        val isPartitionResult = isPartition(predicates, solver)
        val isCoverResult = isCover(predicates, solver)
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

  private def isPartition(predicates: Iterable[Predicate], solver: Z3Solver): Boolean = {
    MathUtils.choose2(predicates).forall({
      case (p1, p2) =>
        // Any two predicate should conflict with each other
        !solver.checkAssertionPushPop(solver.mkAnd(p1.ast, p2.ast), printUnsatCore = false)
    })
  }

  private def isCover(predicates: Iterable[Predicate], solver: Z3Solver): Boolean = {
    solver.checkAssertionPushPop(
      solver.mkIff(solver.mkOr(predicates.toSeq.map(p => p.ast): _*), solver.mkTrue()), printUnsatCore = false)
  }
}

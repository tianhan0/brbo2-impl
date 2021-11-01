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
    val predicates: Set[Predicate] = Predicate.generatePredicates(???, solver)

    def computeNewReset(groupId: Int, indexMap: (Set[Int], Set[Int]), guardOfReset: BrboExpr): Reset = {
      val postConditionKeep = computePostCondition(indexMap._1)
      val impliedPredicatesKeep = computeImpliedPredicates(predicates, postConditionKeep, guardOfReset, solver)
      val postConditionRemove = computePostCondition(indexMap._2)
      val impliedPredicatesRemove = computeImpliedPredicates(predicates, postConditionRemove, guardOfReset, solver)
      logger.traceOrError(s"These predicates may distinguish keeping or removing reset instances in group `$groupId`: `$impliedPredicatesRemove`")
      logger.traceOrError(s"Compute candidate predicates for partitioning resets from old group `$groupId")
      val candidatePredicates = computeCandidatePredicates(List(impliedPredicatesKeep, impliedPredicatesRemove), solver)
      Reset(groupId, And(guardOfReset, candidatePredicates.head.head.expr)) // Only keep the reset under the computed predicate
    }

    def computePostCondition(indices: Set[Int]): AST = {
      val postConditions = indices.map({
        index =>
          val valuation = symbolicExecution.execute(refinement.path.slice(0, index)).valuations.head
          SymbolicExecution.valuationToAST(valuation, solver)
      }).toSeq
      solver.mkOr(postConditions: _*)
    }

    val resetReplacements: Map[Reset, Reset] = refinement.path.zipWithIndex.foldLeft(Map[Reset, Reset]())({
      case (acc, (pathNode, index: Int)) =>
        refinement.splitUses.get(index) match {
          case Some(replace: Replace) =>
            replace match {
              case ResetNode(newReset, _) =>
                val oldResetCommand = pathNode.value.left.asInstanceOf[Reset]
                val newResetCommand = newReset.value.left.asInstanceOf[Reset]
                assert(oldResetCommand.condition == newResetCommand.condition)
                acc + (oldResetCommand -> newResetCommand)
              case UseNode(_, _) => acc
              case _ => throw new Exception
            }
          case None => acc
        }
    })

    val useReplacements: Map[Use, Set[Use]] =
      useCommands.foldLeft(Map[Use, Set[Use]]())({
        (useMap, useCommand) =>
          val use = useCommand.asInstanceOf[Use]
          // A map from new group IDs to indices of uses in the path that belong to the new group
          val indexMap = refinement.getSplitUseInstances(use)
          val candidateGuardMap = indexMap.foldLeft(Map[Int, Set[Predicate]]())({
            case (acc, (groupId, indices)) =>
              val postCondition = computePostCondition(indices)
              val impliedPredicates = computeImpliedPredicates(predicates, postCondition, use.condition, solver)
              logger.traceOrError(s"These predicates may distinguish use instances in new group `$groupId`: `$impliedPredicates`")
              acc + (groupId -> impliedPredicates)
          })

          val sortedIds = candidateGuardMap.keys.toList.sorted
          logger.traceOrError(s"Compute candidate predicates for partitioning old group `${use.groupId.get}")
          val candidatePredicates = computeCandidatePredicates(sortedIds.map(id => candidateGuardMap.getOrElse(id, throw new Exception)), solver)

          val newUses = sortedIds.zip(candidatePredicates.head).map({
            case (groupId, predicate) =>
              val finalPredicate = And(predicate.expr, use.condition)
              logger.traceOrError(s"Split Use: Choose predicate `$finalPredicate` for new group `$groupId` (partitioned from old group `${use.groupId.get})`")
              Use(Some(groupId), use.update, finalPredicate)
          }).toSet

          useMap + (use -> newUses)
      })

    val newResetReplacements = resetCommands.foldLeft(Map[Reset, Reset]())({
      (acc, resetCommand) =>
        val oldReset = resetCommand.asInstanceOf[Reset]
        val newReset: Reset = resetReplacements.get(oldReset) match {
          case Some(replace: Reset) =>
            refinement.groupIDs(oldReset.groupId).foreach({
              newGroupId =>
                val indexMap = refinement.getResetInstances(oldReset, Some(newGroupId))
                logger.traceOrError(s"Compute resets for new group `$newGroupId` (from old group: `${oldReset.groupId})")
                computeNewReset(newGroupId, indexMap, replace.condition)
            })
          case None =>
            val indexMap = refinement.getResetInstances(oldReset, None)
            logger.traceOrError(s"Compute resets for old group `${oldReset.groupId}")
            computeNewReset(oldReset.groupId, indexMap, oldReset.condition)
        }

        acc + (oldReset -> newReset)
    })

    ???
  }

  private def computeImpliedPredicates(predicates: Set[Predicate], whatToImplyFrom: AST,
                                       excludeFromResult: BrboExpr, solver: Z3Solver): Set[Predicate] = {
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

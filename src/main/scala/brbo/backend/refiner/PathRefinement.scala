package brbo.backend.refiner

import brbo.backend.verifier.cex.{Path, Segment}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{CommandLineArguments, MathUtils, MyLogger}

class PathRefinement(arguments: CommandLineArguments) {
  private val maxGroups = arguments.getMaxGroups
  private val logger = MyLogger.createLogger(classOf[PathRefinement], arguments.getDebugMode)

  // Perform command transformations to commands in the given path, and only for commands in the specified function
  def refine(path: Path, targetFunctionName: String): List[Refinement] = {
    logger.infoOrError(s"Refining path: `$path`")
    val useInsertedPaths: Set[Refinement] = replaceUseOnly(path, targetFunctionName)
    useInsertedPaths.flatMap({
      useInsertedPath: Refinement => removeResetOnly(useInsertedPath, targetFunctionName)
    }).toList.sortWith({ case (r1, r2) => r1.toString <= r2.toString })
  }

  def removeResetOnly(refineUseOnly: Refinement, targetFunctionName: String): Set[Refinement] = {
    def helper(numberToKeep: Int, currentRefine: Refinement, currentIndex: Int, remaining: List[CFGNode]): Set[Refinement] = {
      assert(numberToKeep >= 0)
      remaining match {
        case Nil => Set(currentRefine)
        case ::(head, tail) =>
          // Do not transform commands in functions other than the given function
          if (head.functionIdentifier != targetFunctionName)
            return helper(numberToKeep, currentRefine, currentIndex + 1, tail)
          head.value match {
            case Left(command) =>
              command match {
                case Reset(_, _, _) =>
                  val newRefine = currentRefine.removeReset(currentIndex)
                  if (numberToKeep == 0) helper(numberToKeep, newRefine, currentIndex + 1, tail)
                  else {
                    helper(numberToKeep - 1, currentRefine, currentIndex + 1, tail) ++
                      helper(numberToKeep, newRefine, currentIndex + 1, tail)
                  }
                case _ => helper(numberToKeep, currentRefine, currentIndex + 1, tail)
              }
            case Right(_) => helper(numberToKeep, currentRefine, currentIndex + 1, tail)
          }
      }
    }

    val numberOfResets = refineUseOnly.path.count({ node => node.isReset(None, Some(targetFunctionName)) })

    Range.inclusive(0, numberOfResets).toSet.flatMap({
      numberToKeep =>
        helper(numberToKeep,
          Refinement(refineUseOnly.path, refineUseOnly.splitUses, Set(), refineUseOnly.groupIds), currentIndex = 0, refineUseOnly.path)
    })
  }

  def replaceUseOnly(path: Path, targetFunctionName: String): Set[Refinement] = {
    val pathIndices = path.pathNodes.indices

    val groupIds: List[Int] = {
      var groupIds = Set[Int]()
      path.pathNodes.foreach({
        node =>
          node.value match {
            case Left(command) =>
              command match {
                case Use(groupID, _, _, _) => groupIds = groupIds + groupID.get
                case Reset(groupID, _, _) => groupIds = groupIds + groupID
                case _ =>
              }
            case Right(_) =>
          }
      })
      groupIds.toList.sorted
    }
    logger.traceOrError(s"All groups: `$groupIds`")

    val allSegments: Map[Int, List[Segment]] = groupIds.foldLeft(Map[Int, List[Segment]]())({
      (acc, groupId) => acc + (groupId -> path.getSegments(groupId, targetFunctionName))
    })

    def helper(existingGroups: Set[Int], currentRefine: Refinement, toSplitGroupIds: List[Int]): Set[Refinement] = {
      logger.traceOrError(s"existingGroups: `$existingGroups`, toSplitGroupIds: `$toSplitGroupIds`, currentRefine: `${currentRefine.toStringNoPath}`")
      if (toSplitGroupIds.isEmpty) return Set(currentRefine)
      val toSplitGroupId = toSplitGroupIds.head
      val segments = allSegments.getOrElse(toSplitGroupId, throw new Exception)
      // Not consider the first segment, because BrboFunction and Path both guarantees that, the accumulation in the first segment
      // of any group is 0, because any concrete path begins with initializatin ghost variables
      // Hence, it does not really matter we "assign" the first segment to which new group
      val segmentSizeMinusOne = segments.size - 1
      var result: Set[Refinement] = helper(existingGroups, currentRefine, toSplitGroupIds.tail) // Not split the group
      val minGroupId = existingGroups.max + 1
      val maxGroupId = {
        val a = existingGroups.max + segmentSizeMinusOne
        val b = maxGroups - existingGroups.size + existingGroups.max
        if (a <= b) a else b
      }
      logger.traceOrError(s"Number of segments minus 1: `$segmentSizeMinusOne`, minGroupId: `$minGroupId`, maxGroupId: `$maxGroupId` (maxGroups: `$maxGroups`)")
      val possibilities: Set[List[Int]] = MathUtils.generateUniqueSequences(segmentSizeMinusOne, minGroupId, maxGroupId)
      result = result ++ possibilities.flatMap({
        possibility =>
          val oneNewGroup = possibility.min == possibility.max
          if (oneNewGroup) {
            // If the number of new groups is 1, then we are actually not splitting!
            logger.traceOrError(s"Possibility: `$possibility` (Skipped)")
            Set[Refinement]()
          }
          else {
            logger.traceOrError(s"Possibility: `$possibility`")
            assert(possibility.size == segmentSizeMinusOne)
            val splitUses = segments.tail.zip(possibility).foldLeft(currentRefine.splitUses)({
              case (acc, (segment, newGroupId)) =>
                pathIndices.slice(segment.begin, segment.end + 1).foldLeft(acc)({
                  case (acc2, pathIndex) =>
                    // Every node in the segment now "belongs to" the new group
                    val node = path.pathNodes(pathIndex)
                    assert(!acc2.contains(pathIndex))
                    // Do not transform commands in functions other than the target function
                    if (node.isReset(Some(toSplitGroupId), Some(targetFunctionName))) {
                      val newReset = CFGNode(Left(node.value.left.get.asInstanceOf[Reset].replace(newGroupId)), None, CFGNode.DONT_CARE_ID)
                      acc2 + (pathIndex -> ResetNode(newReset, newGroupId))
                    }
                    else if (node.isUse(Some(toSplitGroupId), Some(targetFunctionName))) {
                      val newUse = CFGNode(Left(node.value.left.get.asInstanceOf[Use].replace(newGroupId)), None, CFGNode.DONT_CARE_ID)
                      acc2 + (pathIndex -> UseNode(newUse, newGroupId))
                    }
                    else acc2
                })
            })
            val newRefine = Refinement(currentRefine.path, splitUses, currentRefine.removeResets, currentRefine.groupIds + (toSplitGroupId -> possibility.toSet))
            helper(existingGroups - toSplitGroupId ++ possibility.toSet, newRefine, toSplitGroupIds.tail)
          }
      })
      result
    }

    helper(groupIds.toSet, Refinement(path.pathNodes, Map(), Set(), Map()), groupIds)
  }
}

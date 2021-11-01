package brbo.backend.refiner

import brbo.backend.verifier.cex.{Path, Segment}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{CommandLineArguments, MathUtils, MyLogger}

class PathRefinement(commandLineArguments: CommandLineArguments, targetFunction: BrboFunction) {
  private val maxGroups = commandLineArguments.getMaxGroups
  private val logger = MyLogger.createLogger(classOf[PathRefinement], commandLineArguments.getDebugMode)

  // Perform command transformations to commands in the given path
  def refine(path: Path): List[Refinement] = {
    val useInsertedPaths: Set[Refinement] = replaceUseOnly(path)
    useInsertedPaths.flatMap({
      useInsertedPath: Refinement => removeResetOnly(useInsertedPath)
    }).toList.sortWith({ case (r1, r2) => r1.toString <= r2.toString })
  }

  def removeResetOnly(refineUseOnly: Refinement): Set[Refinement] = {
    def helper(numberToKeep: Int, currentRefine: Refinement, currentIndex: Int, remaining: List[CFGNode]): Set[Refinement] = {
      assert(numberToKeep >= 0)
      remaining match {
        case Nil => Set(currentRefine)
        case ::(head, tail) =>
          // Do not transform commands in functions other than the given function
          if (head.function.identifier != targetFunction.identifier)
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

    val numberOfResets = refineUseOnly.path.count({ node => node.isReset(None, Some(targetFunction)) })

    Range.inclusive(0, numberOfResets).toSet.flatMap({
      numberToKeep => helper(numberToKeep, Refinement(Nil, refineUseOnly.splitUses, Set(), refineUseOnly.groupIDs), currentIndex = 0, refineUseOnly.path)
    })
  }

  def replaceUseOnly(path: Path): Set[Refinement] = {
    val pathWithIndex = path.pathNodes.zipWithIndex

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

    val allSegments: Map[Int, List[Segment]] = {
      groupIds.foldLeft(Map[Int, List[Segment]]())({
        (acc, groupId) => acc + (groupId -> path.getSegments(groupId, targetFunction))
      })
    }

    def helper(exitingGroups: Set[Int], currentRefine: Refinement, toSplitGroupIds: List[Int]): Set[Refinement] = {
      logger.traceOrError(s"exitingGroups: `$exitingGroups`, toSplitGroupIds: `$toSplitGroupIds`, currentRefine: `${currentRefine.toStringNoPath}`")
      if (toSplitGroupIds.isEmpty) return Set(currentRefine)
      val toSplitGroupId = toSplitGroupIds.head
      val segments = allSegments.getOrElse(toSplitGroupId, throw new Exception)
      var result: Set[Refinement] = helper(exitingGroups, currentRefine, toSplitGroupIds.tail) // Not split the group
      val minGroupId = exitingGroups.max + 1
      val maxGroupId = {
        val a = exitingGroups.max + segments.size
        val b = maxGroups - exitingGroups.size + exitingGroups.max
        if (a <= b) a else b
      }
      logger.traceOrError(s"Number of segments: `${segments.size}`, minGroupId: `$minGroupId`, maxGroupId: `$maxGroupId`")
      Range.inclusive(minGroupId, maxGroupId).foreach({
        currentMaxGroupId =>
          logger.traceOrError(s"minGroupId: `$minGroupId`, currentMaxGroupId: `$currentMaxGroupId`")
          val possibilities: Set[List[Int]] = MathUtils.generateUniqueSequences(segments.size, minGroupId, currentMaxGroupId)
          result = result ++ possibilities.flatMap({
            possibility =>
              val oneNewGroup = possibility.min == possibility.max
              // If the number of new groups is 1, then we are actually not splitting!
              if (oneNewGroup) {
                Set[Refinement]()
              }
              else {
                logger.traceOrError(s"Possibility: `$possibility`")
                assert(possibility.size == segments.size)
                val splitUses = segments.zip(possibility).foldLeft(currentRefine.splitUses)({
                  case (acc, (segment, newGroupId)) =>
                    pathWithIndex.slice(segment.begin, segment.end + 1).foldLeft(acc)({
                      case (acc2, (node, index)) =>
                        assert(!acc2.contains(index))
                        // Do not transform commands in functions other than the main function
                        if (node.isReset(Some(toSplitGroupId), Some(targetFunction))) {
                          val newReset = CFGNode(Left(node.value.left.asInstanceOf[Reset].replace(newGroupId)), targetFunction, CFGNode.DONT_CARE_ID)
                          acc2 + (index -> ResetNode(newReset, newGroupId))
                        }
                        else if (node.isUse(Some(toSplitGroupId), Some(targetFunction))) {
                          val update = node.value.left.get.asInstanceOf[Use].update
                          val newUse = CFGNode(Left(node.value.left.asInstanceOf[Use].replace(newGroupId)), targetFunction, CFGNode.DONT_CARE_ID)
                          acc2 + (index -> UseNode(newUse, newGroupId))
                        }
                        else acc2
                    })
                })
                val newRefine = Refinement(currentRefine.path, splitUses, currentRefine.removeResets, currentRefine.groupIDs + (toSplitGroupId -> possibility.toSet))
                helper(exitingGroups - toSplitGroupId ++ possibility.toSet, newRefine, toSplitGroupIds.tail)
              }
          })
      })
      result
    }

    helper(groupIds.toSet, Refinement(path.pathNodes, Map(), Set(), Map()), groupIds)
  }
}

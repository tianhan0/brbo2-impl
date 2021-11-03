package brbo.backend.refiner

import brbo.backend.refiner.Refinement.logger
import brbo.common.{GhostVariableUtils, MyLogger}
import brbo.common.ast.{BrboFunction, Reset, Use}
import brbo.common.cfg.CFGNode
import brbo.frontend.JavacUtils

/**
 *
 * @param path         A path that this refinement applies to.
 * @param splitUses    A map from indices of the path to the replacements for the nodes at the indices.
 *                     The replacement is either a use command or a reset command for a new group.
 * @param removeResets Remove the resets at these indices from the path after splitting uses.
 * @param groupIds     A map from old group IDs to new group IDs in the refined path.
 */
case class Refinement(path: List[CFGNode], splitUses: Map[Int, Replace], removeResets: Set[Int], groupIds: Map[Int, Set[Int]]) {
  def noRefinement: Boolean = splitUses.isEmpty && groupIds.isEmpty

  def refinedPath(whereToInitializeGhostVariables: BrboFunction): List[CFGNode] = {
    val newGroupInitializations = groupIds.values.flatten.flatMap({
      groupId => GhostVariableUtils.declareVariables(groupId).map(c => CFGNode(Left(c), whereToInitializeGhostVariables, CFGNode.DONT_CARE_ID))
    }).toList.sortWith({ case (n1, n2) => n1.value.left.get.toIR() < n2.value.left.get.toIR() })
    // logger.traceOrError(s"Path (length `${path.size}`):\n`$path`")
    val afterSplit: List[CFGNode] = splitUses.foldLeft(path)({
      case (acc, (i, replacement)) => acc.updated(i, replacement.newNode)
    })
    var result: List[CFGNode] = Nil
    afterSplit.indices.foreach({
      i => if (!removeResets.contains(i)) result = afterSplit(i) :: result
    })
    newGroupInitializations ::: result.reverse
  }

  // A map from new group IDs to indices of use instances (of the given use command) in the path that
  // belong to the new group
  def getSplitUseInstances(useInOriginalPath: Use): Map[Int, Set[Int]] = {
    groupIds.get(useInOriginalPath.groupId.get) match {
      case Some(newGroupIds) => // This use splits
        var map: Map[Int, Set[Int]] = Map()
        path.indices.foreach({
          i =>
            path(i).value match {
              case Left(command) =>
                if (command == useInOriginalPath) {
                  splitUses.get(i) match {
                    case Some(replace: Replace) =>
                      replace match {
                        case UseNode(_, groupId) =>
                          assert(newGroupIds.contains(groupId))
                          map.get(groupId) match {
                            case Some(set) => map = map + (groupId -> (set + i))
                            case None => map = map + (groupId -> Set(i))
                          }
                        case _ =>
                      }
                    case None =>
                  }
                } // Otherwise, this is a use instance that belongs to the same group but does not correspond to the given use command
              case Right(_) =>
            }
        })
        assert(map.keySet == newGroupIds, s"keySet: `${map.keySet}`. newGroupIds: `$newGroupIds`")
        map
      case None => Map[Int, Set[Int]]() // This use does not split
    }
  }

  // A map from group IDs to indices of reset instances (of the given reset command) in the path that
  // belong to the group and are kept and removed
  def getResetInstances(resetInOriginalPath: Reset): Map[Int, (Set[Int], Set[Int])] = {
    val thisGroupId = resetInOriginalPath.groupId
    val result = path.indices.foldLeft(Map[Int, (Set[Int], Set[Int])]())({
      (acc, i) =>
        path(i).value match {
          case Left(command) =>
            if (command == resetInOriginalPath) {
              val newGroupId: Int = splitUses.get(i) match {
                case Some(replace: Replace) =>
                  replace match {
                    case ResetNode(_, newGroupId) => newGroupId // Replaced
                    case _ => thisGroupId // Not replaced
                  }
                case None => thisGroupId // Not replaced
              }
              val (keepSet: Set[Int], removeSet: Set[Int]) = acc.get(thisGroupId) match {
                case Some((keepSet, removeSet)) => (keepSet, removeSet)
                case None => (Set(), Set())
              }
              val (newKeepSet, newRemoveSet) = {
                if (removeResets.contains(i)) (keepSet, removeSet + i)
                else (keepSet + i, removeSet)
              }
              acc + (newGroupId -> (newKeepSet, newRemoveSet))
            }
            else acc
          case Right(_) => acc
        }
    })
    result.size match {
      case 0 =>
      case 1 => assert(result.head._1 == thisGroupId) // The given reset command is not split
      case _ => result.keys.toSet.subsetOf(groupIds(thisGroupId)) // The given reset command is split
    }
    result
  }

  def removeReset(index: Int): Refinement = Refinement(path, splitUses, removeResets + index, groupIds)

  private val separator = "\n  "
  private val removedString = {
    val s = removeResets.toList.sorted.mkString(separator)
    s"Removed resets:$separator$s"
  }
  private val splitsString = {
    val s = splitUses.toList.map({
      case (index, replace) =>
        s"$index: $replace"
    }).sorted.mkString(separator)
    s"Splits:$separator$s"
  }

  override def toString: String = {
    val pathString = {
      val s = path.map({ node => node.toString }).mkString(separator)
      s"Path:$separator$s"
    }
    s"$pathString\n$splitsString\n$removedString"
  }

  def toStringNoPath: String = s"$splitsString\n$removedString"
}

object Refinement {
  private val logger = MyLogger.createLogger(Refinement.getClass, debugMode = true)
}

abstract class Replace(val newNode: CFGNode, val newGroupId: Int)

case class UseNode(newUse: CFGNode, override val newGroupId: Int) extends Replace(newUse, newGroupId) {
  val use: Use = newUse.value.left.get.asInstanceOf[Use]

  override def toString: String = use.prettyPrintToCFG
}

case class ResetNode(newReset: CFGNode, override val newGroupId: Int) extends Replace(newReset, newGroupId) {
  val reset: Reset = newReset.value.left.get.asInstanceOf[Reset]

  override def toString: String = reset.prettyPrintToCFG
}
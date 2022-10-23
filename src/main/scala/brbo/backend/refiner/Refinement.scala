package brbo.backend.refiner

import brbo.backend.refiner.Refinement._
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.string.StringFormatUtils
import brbo.common.{GhostVariableUtils, MyLogger, SameAs}

/**
 *
 * @param path         A path that this refinement applies to.
 * @param splitUses    A map from indices of the path to the replacements for the nodes at the indices.
 *                     The replacement is either a use command or a reset command for a new group.
 * @param removeResets Remove the resets at these indices from the path **after** splitting uses.
 * @param groupIds     A map from old group IDs to new group IDs in the refined path.
 */
case class Refinement(path: List[CFGNode], splitUses: Map[Int, Replace], removeResets: Set[Int],
                      groupIds: Map[Int, Set[Int]]) extends SameAs {
  def noRefinement: Boolean = splitUses.isEmpty && removeResets.isEmpty && groupIds.isEmpty

  private val splitUsesList = replaceMapToList(splitUses)

  def sameAs(other: Any): Boolean = {
    other match {
      case Refinement(otherPath, otherSplitUses, otherRemoveResets, _) =>
        val samePath =
          if (otherPath.length != this.path.length) false
          else this.path.zip(otherPath).forall({ case (n1, n2) => n1.sameValue(n2) })
        val sameSplitUses = {
          val otherList = replaceMapToList(otherSplitUses)
          if (splitUsesList.length != otherList.length) false
          else splitUsesList.zip(otherList).forall({ case (p1, p2) => p1._1 == p2._1 && p1._2.sameAs(p2._2) })
        }
        samePath && sameSplitUses && otherRemoveResets == removeResets
      case _ => false
    }
  }

  def refinedPath(whereToInitializeGhostVariables: BrboFunction): List[CFGNode] = {
    val newGroupInitializations = groupIds.values.flatten.flatMap({
      groupId => GhostVariableUtils.declareVariables(groupId).map(c => CFGNode(c, Some(whereToInitializeGhostVariables), CFGNode.DONT_CARE_ID))
    }).toList.sortWith({ case (n1, n2) => n1.command.asInstanceOf[Command].printToC(0) < n2.command.asInstanceOf[Command].printToC(0) })
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
            path(i).command match {
              case command: Command =>
                if (command == useInOriginalPath) {
                  // Assume the given command and the cex. path are generated from the same program, because their uuids are the same
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
              case _: BrboExpr =>
            }
        })
        assert(map.keySet.subsetOf(newGroupIds), s"Find instances for use `${useInOriginalPath.toIR(0)}`. newGroupIds: `${map.keySet}`. allNewGroupIds: `$newGroupIds`")
        map
      case None => Map[Int, Set[Int]]() // This use does not split
    }
  }

  // A map from group IDs to indices of reset instances (of the given reset command) in the path that
  // belong to the group and are kept and removed
  def getResetInstances(resetInOriginalPath: Reset): Map[Int, (Set[Int], Set[Int])] = {
    val thisGroupId = resetInOriginalPath.groupId
    path.indices.foldLeft(Map[Int, (Set[Int], Set[Int])]())({
      (acc, i) =>
        path(i).command match {
          case command: Command =>
            if (command == resetInOriginalPath) {
              // Assume the given command and the cex. path are generated from the same program, because their uuids are the same
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
          case _: BrboExpr => acc
        }
    })
  }

  def removeReset(index: Int): Refinement = Refinement(path, splitUses, removeResets + index, groupIds)

  private val separator = "\n  "
  private val removedString = {
    val s = removeResets.toList.sorted.map(index => s"[${StringFormatUtils.integer(index)}]: ${path(index)}").mkString(separator)
    s"Removed resets:$separator$s"
  }
  private val splitsString = {
    val s = splitUses.toList.map({
      case (index, replace) =>
        s"[${StringFormatUtils.integer(index)}] ${path(index)} -> $replace"
    }).sorted.mkString(separator)
    s"Splits:$separator$s"
  }

  override def toString: String = {
    val pathString = PrintPath.pathToString(path)
    s"$pathString\n$splitsString\n$removedString"
  }

  def toStringNoPath: String = s"$splitsString\n$removedString"

  private def replaceMapToList(map: Map[Int, Replace]): List[(Int, Replace)] =
    map.toList.sortWith({ case (p1, p2) => p1._1 < p2._1 })
}

object Refinement {
  private val logger = MyLogger.createLogger(Refinement.getClass, debugMode = true)

  abstract class Replace(val newNode: CFGNode, val newGroupId: Int) extends SameAs

  case class UseNode(newUse: CFGNode, override val newGroupId: Int) extends Replace(newUse, newGroupId) {
    val use: Use = newUse.command.asInstanceOf[Use]

    override def toString: String = use.printToIR

    override def sameAs(other: Any): Boolean = {
      other match {
        case UseNode(otherNewUse, otherNewGroupId) =>
          otherNewUse.sameValue(newUse) && otherNewGroupId == newGroupId
        case _ => false
      }
    }
  }

  case class ResetNode(newReset: CFGNode, override val newGroupId: Int) extends Replace(newReset, newGroupId) {
    val reset: Reset = newReset.command.asInstanceOf[Reset]

    override def toString: String = reset.printToIR

    override def sameAs(other: Any): Boolean = {
      other match {
        case ResetNode(otherNewReset, otherNewGroupId) =>
          otherNewReset.sameValue(newReset) && otherNewGroupId == newGroupId
        case _ => false
      }
    }
  }
}
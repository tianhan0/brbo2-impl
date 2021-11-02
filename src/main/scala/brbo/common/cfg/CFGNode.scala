package brbo.common.cfg

import brbo.common.GhostVariableUtils
import brbo.common.ast._
import com.ibm.wala.util.graph.INodeWithNumberedEdges
import com.ibm.wala.util.graph.impl.NodeWithNumber
import com.ibm.wala.util.intset.{BimodalMutableIntSet, IntSet}

/**
 *
 * @param value    Every node is either a command or an expression
 * @param function The function that this command or expression belongs to
 * @param id       A unique ID among all commands and expressions in all functions
 */
case class CFGNode(value: Either[Command, BrboExpr], function: BrboFunction, id: Int)
  extends NodeWithNumber with INodeWithNumberedEdges
    with PrettyPrintToC with PrettyPrintToCFG with GetFunctionCalls {

  private val predNumbers = new BimodalMutableIntSet()
  private val succNumbers = new BimodalMutableIntSet()

  override def getGraphNodeId: Int = id

  override def addPred(n: Int): Unit = predNumbers.add(n)

  override def addSucc(n: Int): Unit = succNumbers.add(n)

  override def getPredNumbers: IntSet = predNumbers

  override def getSuccNumbers: IntSet = succNumbers

  override def removeIncomingEdges(): Unit = throw new Exception

  override def removeOutgoingEdges(): Unit = throw new Exception

  override def removeAllIncidentEdges(): Unit = throw new Exception

  override def toString: String = {
    value match {
      case Left(command) => s"${command.prettyPrintToCFG} [Function `${function.identifier}`]"
      case Right(expr) => s"${expr.prettyPrintToCFG} [Function `${function.identifier}`]"
    }
  }

  override def prettyPrintToC(indent: Int): String = {
    value match {
      case Left(command) => s"${command.prettyPrintToC()}"
      case Right(expr) => s"${expr.prettyPrintToC()}"
    }
  }

  override def getFunctionCalls: List[FunctionCallExpr] = {
    value match {
      case Left(command) => command.getFunctionCalls
      case Right(expr) => expr.getFunctionCalls
    }
  }

  override def prettyPrintToCFG: String = {
    value match {
      case Left(command) => s"($id) ${command.prettyPrintToCFG}"
      case Right(expr) => s"($id) ${expr.prettyPrintToCFG}"
    }
  }

  // Whether this node is a use command, or a use command for the given group in the given function
  def isUse(groupId: Option[Int], inFunction: Option[BrboFunction]): Boolean = {
    inFunction match {
      case Some(function2) => if (function2.identifier != function.identifier) return false
      case None =>
    }

    value match {
      case Left(command) =>
        command match {
          case Use(groupId2, _, _, _) =>
            (groupId2, groupId) match {
              case (Some(value2), Some(value)) => value2 == value
              case (Some(_), None) => true
              case (None, Some(_)) => false
              case (None, None) => true
            }
          case _ => false
        }
      case Right(_) => false
    }
  }

  // Whether this node is a reset command, or a reset command for the given group in the given function
  def isReset(groupId: Option[Int], inFunction: Option[BrboFunction]): Boolean = {
    inFunction match {
      case Some(function2) => if (function2.identifier != function.identifier) return false
      case None =>
    }

    value match {
      case Left(command) =>
        command match {
          case Reset(groupID2, _, _) =>
            groupId match {
              case Some(value) => groupID2 == value
              case None => true
            }
          case _ => false
        }
      case Right(_) => false
    }
  }
}

object CFGNode {
  val DONT_CARE_ID: Int = -1
}
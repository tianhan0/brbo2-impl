package brbo.common.cfg

import brbo.common.ast._
import com.ibm.wala.util.graph.INodeWithNumberedEdges
import com.ibm.wala.util.graph.impl.NodeWithNumber
import com.ibm.wala.util.intset.{BimodalMutableIntSet, IntSet}

/**
 *
 * @param value    Every node is either a command or an expression
 * @param function The function that this command or expression belongs to (if any)
 * @param id       A unique (if desired) ID among all commands and expressions in all functions
 */
case class CFGNode(value: CommandOrExpr, function: Option[BrboFunction] = None, id: Int = CFGNode.DONT_CARE_ID)
  extends NodeWithNumber with INodeWithNumberedEdges with PrettyPrintToC with PrettyPrintToCFG with GetFunctionCalls {
  private val predNumbers = new BimodalMutableIntSet()
  private val succNumbers = new BimodalMutableIntSet()
  val functionIdentifier: String = function match {
    case Some(f) => f.identifier
    case None => CFGNode.FUNCTION_NAME_WHEN_FUNCTION_NOT_EXIST
  }

  def replaceCopy(newValue: CommandOrExpr): CFGNode = CFGNode(newValue, function, id)

  def printWithUUID(): String = s"${prettyPrintToC()} ${CommandOrExpr.extractUUID(value)}"

  def sameValue(other: CFGNode): Boolean = other.value.sameAs(value)

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
      case expr: BrboExpr => s"($id) ${expr.prettyPrintToCFG} [fun `$functionIdentifier`]"
      case command: Command => s"($id) ${command.toIR()} [fun `$functionIdentifier`]"
    }
  }

  override def prettyPrintToC(indent: Int): String = {
    value match {
      case command: Command => s"${command.prettyPrintToC()}"
      case expr: BrboExpr => s"${expr.prettyPrintToC()}"
    }
  }

  override def getFunctionCalls: List[FunctionCallExpr] = {
    value match {
      case command: Command => command.getFunctionCalls
      case expr: BrboExpr => expr.getFunctionCalls
    }
  }

  override def prettyPrintToCFG: String = s"($id) ${value.prettyPrintToCFG}"

  def simplifiedString: String = value.prettyPrintToCFG

  // Whether this node is a use command, or a use command for the given group in the given function
  def isUse(groupId: Option[Int], functionName: Option[String]): Boolean = {
    functionName match {
      case Some(name) => if (name != functionIdentifier) return false
      case None =>
    }

    value match {
      case command: Command =>
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
      case _: BrboExpr => false
    }
  }

  // Whether this node is a reset command, or a reset command for the given group in the given function
  def isReset(groupId: Option[Int], functionName: Option[String]): Boolean = {
    functionName match {
      case Some(name) => if (name != functionIdentifier) return false
      case None =>
    }

    value match {
      case command: Command =>
        command match {
          case Reset(groupID2, _, _) =>
            groupId match {
              case Some(value) => groupID2 == value
              case None => true
            }
          case _ => false
        }
      case _: BrboExpr => false
    }
  }
}

object CFGNode {
  val DONT_CARE_ID: Int = -1
  val FUNCTION_NAME_WHEN_FUNCTION_NOT_EXIST = "NONE!"
}
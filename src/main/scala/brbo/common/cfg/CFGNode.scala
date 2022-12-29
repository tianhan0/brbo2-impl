package brbo.common.cfg

import brbo.common.ast._
import com.ibm.wala.util.graph.INodeWithNumberedEdges
import com.ibm.wala.util.graph.impl.NodeWithNumber
import com.ibm.wala.util.intset.{BimodalMutableIntSet, IntSet}

/**
 *
 * @param command  Every node is either a command or an expression
 * @param function The function that this command or expression belongs to (if any)
 * @param id       A unique (if desired) ID among all commands and expressions in all functions
 */
case class CFGNode(command: Command, function: Option[BrboFunction] = None, id: Int = CFGNode.DONT_CARE_ID)
  extends NodeWithNumber with INodeWithNumberedEdges
    with GetFunctionCalls with Serializable {
  private val predNumbers = new BimodalMutableIntSet()
  private val succNumbers = new BimodalMutableIntSet()
  val functionIdentifier: String = function match {
    case Some(f) => f.identifier
    case None => CFGNode.FUNCTION_NAME_WHEN_FUNCTION_NOT_EXIST
  }

  def replaceCopy(newValue: Command): CFGNode = CFGNode(newValue, function, id)

  def sameValue(other: CFGNode): Boolean = other.command.sameAs(command)

  override def getGraphNodeId: Int = id

  override def addPred(n: Int): Unit = predNumbers.add(n)

  override def addSucc(n: Int): Unit = succNumbers.add(n)

  override def getPredNumbers: IntSet = predNumbers

  override def getSuccNumbers: IntSet = succNumbers

  override def removeIncomingEdges(): Unit = throw new Exception

  override def removeOutgoingEdges(): Unit = throw new Exception

  override def removeAllIncidentEdges(): Unit = throw new Exception

  override def toString: String = {
    s"($id) ${command.printToIR()} [fun `$functionIdentifier`]"
  }

  def printToIR(): String = s"($id) ${command.printToIR()}"

  override def getFunctionCalls: List[FunctionCallExpr] = {
    command.getFunctionCalls
  }

  def simplifiedString: String = command.printToIR()

  // Whether this node is a use command, or a use command for the given group in the given function
  def isUse(groupId: Option[Int], functionName: Option[String]): Boolean = {
    functionName match {
      case Some(name) => if (name != functionIdentifier) return false
      case None =>
    }

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
  }

  // Whether this node is a reset command, or a reset command for the given group in the given function
  def isReset(groupId: Option[Int], functionName: Option[String]): Boolean = {
    functionName match {
      case Some(name) => if (name != functionIdentifier) return false
      case None =>
    }

    command match {
      case Reset(groupID2, _, _) =>
        groupId match {
          case Some(value) => groupID2 == value
          case None => true
        }
      case _ => false
    }
  }
}

object CFGNode {
  val DONT_CARE_ID: Int = -1
  val FUNCTION_NAME_WHEN_FUNCTION_NOT_EXIST = "NONE!"

  def copyNodeOnly(node: CFGNode): CFGNode = CFGNode(node.command, node.function, node.id)
}
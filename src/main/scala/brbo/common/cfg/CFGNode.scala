package brbo.common.cfg

import brbo.common.ast.{BrboExpr, BrboFunction, Command}
import com.ibm.wala.util.graph.INodeWithNumberedEdges
import com.ibm.wala.util.graph.impl.NodeWithNumber
import com.ibm.wala.util.intset.{BimodalMutableIntSet, IntSet}

/**
 *
 * @param value        Every node is either a command or an expression
 * @param functionName The function that this command or expression belongs to
 * @param id           A unique ID among all commands and expressions in all functions
 */
case class CFGNode(value: Either[Command, BrboExpr], functionName: String, id: Int) extends NodeWithNumber with INodeWithNumberedEdges {
  private val predNumbers = new BimodalMutableIntSet()
  private val succNumbers = new BimodalMutableIntSet()

  override def getGraphNodeId: Int = id

  override def addPred(n: Int): Unit = predNumbers.add(n)

  override def addSucc(n: Int): Unit = succNumbers.add(n)

  override def getPredNumbers: IntSet = predNumbers

  override def getSuccNumbers: IntSet = succNumbers

  override def removeIncomingEdges(): Unit = ???

  override def removeOutgoingEdges(): Unit = ???

  override def removeAllIncidentEdges(): Unit = ???

  override def toString: String = {
    value match {
      case Left(command) => s"($id) ${command.prettyPrintPrintToCFG}"
      case Right(expr) => s"($id) ${expr.prettyPrintPrintToCFG}"
    }
  }
}
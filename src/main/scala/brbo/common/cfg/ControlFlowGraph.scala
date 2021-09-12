package brbo.common.cfg

import brbo.common.ast._
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.impl.DelegatingNumberedGraph
import org.jgrapht.graph.{DefaultEdge, SimpleDirectedGraph}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

case class InternalGraph(root: CFGNode, exits: Set[CFGNode])

object ControlFlowGraph {

  case class JumpTarget(immediateLoopCondition: Option[CFGNode], immediateLoopExit: Option[CFGNode], functionExit: CFGNode)

  /**
   *
   * @param brboProgram Construct a CFG that connects all CFGs of every function in this program
   * @return
   */
  def toControlFlowGraph(brboProgram: BrboProgram): ControlFlowGraph = {
    var nodes = new HashMap[Either[Command, BrboExpr], CFGNode]
    var cfgs = new HashMap[BrboFunction, InternalGraph]
    val walaGraph = new DelegatingNumberedGraph[CFGNode]()
    val jgraphtGraph = new SimpleDirectedGraph[CFGNode, DefaultEdge](classOf[DefaultEdge])

    def addNode(node: CFGNode): Unit = {
      walaGraph.addNode(node)
      jgraphtGraph.addVertex(node)
    }

    def addEdge(src: CFGNode, dst: CFGNode): Unit = {
      walaGraph.addEdge(src, dst)
      jgraphtGraph.addEdge(src, dst)
    }

    def getCFGFromName(functionName: String): InternalGraph = {
      (brboProgram.mainFunction :: brboProgram.functions).find(function => function.identifier == functionName) match {
        case Some(function) => getCFG(function)
        case None => throw new Exception
      }
    }

    def getCFG(brboFunction: BrboFunction): InternalGraph = {
      val functionCFG = functionToInternalGraph(brboFunction)
      cfgs = cfgs + (brboFunction -> functionCFG)
      functionCFG
    }

    def getNode(content: Either[Command, BrboExpr], brboFunction: BrboFunction): CFGNode = {
      nodes.get(content) match {
        case Some(node) => node
        case None =>
          val id = nodes.size + 1
          val node = CFGNode(content, brboFunction, id)
          nodes = nodes + (content -> node)
          addNode(node)
          node
      }
    }

    def functionToInternalGraph(brboFunction: BrboFunction): InternalGraph = {
      val exitNode = getNode(Left(FunctionExit()), brboFunction)
      val internalGraph = astToInternalGraph(brboFunction.body, JumpTarget(None, None, exitNode), brboFunction)
      assert(internalGraph.exits.size == 1 && internalGraph.exits.head == exitNode, "Any function has exactly one exit node")
      internalGraph
    }

    def astToInternalGraph(ast: BrboAst, jumpTarget: JumpTarget, brboFunction: BrboFunction): InternalGraph = {
      def addEdgesFromExitsToEntry(exits: Set[CFGNode], entry: CFGNode): Unit = {
        @tailrec
        def shouldAddEdge(nodeValue: Either[Command, BrboExpr]): Boolean = {
          nodeValue match {
            case Left(command) =>
              command match {
                case ReturnExpr(_) | ReturnVoid() | Break() | Continue() => false
                case LabeledCommand(_, command2) => shouldAddEdge(Left(command2))
                case Assert(_) | Assume(_) | VariableDeclaration(_, _, _) | Assignment(_, _) | Skip() | FunctionCall(_, _) => true
                case LoopExit() | FunctionExit() => throw new Exception("LoopExit and FunctionExit are only used in Control Flow Graphs!")
              }
            case Right(_) => true
          }
        }

        exits.foreach({ exit => if (shouldAddEdge(exit.value)) addEdge(exit, entry) })
      }

      ast match {
        case command: Command =>
          val node = getNode(Left(command), brboFunction)
          addJumpEdges(command)

          @tailrec
          def addJumpEdges(command: Command): Unit = {
            command match {
              case Assert(_) | Assume(_) | VariableDeclaration(_, _, _) | Assignment(_, _) | Skip() =>
              case FunctionCall(_, functionCallExpr) =>
                val functionCFG = getCFGFromName(functionCallExpr.identifier)
                addEdge(node, functionCFG.root)
                functionCFG.exits.foreach(exit => addEdge(exit, node)) // TODO: When traversing, need to pair call and return edges.
              case ReturnExpr(_) | ReturnVoid() => addEdge(node, jumpTarget.functionExit)
              case Break() => addEdge(node, jumpTarget.immediateLoopExit.get)
              case Continue() => addEdge(node, jumpTarget.immediateLoopCondition.get)
              case LabeledCommand(_, command2) => addJumpEdges(command2)
              case LoopExit() | FunctionExit() => throw new Exception("LoopExit and FunctionExit are only used in Control Flow Graphs!")
            }
          }

          InternalGraph(node, Set(node))
        case statement: Statement =>
          statement match {
            case Block(statements) =>
              val internalGraphs = statements.map(statement => astToInternalGraph(statement, jumpTarget, brboFunction))
              var i = 0
              while (i < internalGraphs.size) {
                if (i > 0) {
                  val previous = internalGraphs(i - 1)
                  val current = internalGraphs(i)
                  addEdgesFromExitsToEntry(previous.exits, current.root)
                }
                i = i + 1
              }
              assert(internalGraphs.nonEmpty)
              InternalGraph(internalGraphs.head.root, internalGraphs.last.exits)
            case ITE(condition, thenAst, elseAst) =>
              val conditionNode = getNode(Right(condition), brboFunction)
              addNode(conditionNode)

              val thenGraph = astToInternalGraph(thenAst, jumpTarget, brboFunction)
              val elseGraph = astToInternalGraph(elseAst, jumpTarget, brboFunction)

              addEdge(conditionNode, thenGraph.root)
              addEdge(conditionNode, elseGraph.root)

              InternalGraph(conditionNode, thenGraph.exits ++ elseGraph.exits)
            case Loop(condition, body) =>
              val conditionNode = getNode(Right(condition), brboFunction)
              val loopExit = getNode(Left(LoopExit()), brboFunction)
              val bodyGraph = astToInternalGraph(body, JumpTarget(Some(conditionNode), Some(loopExit), jumpTarget.functionExit), brboFunction)

              addEdge(conditionNode, loopExit)
              addEdge(conditionNode, bodyGraph.root)
              addEdgesFromExitsToEntry(bodyGraph.exits, conditionNode)

              InternalGraph(conditionNode, Set(loopExit))
          }
      }
    }

    val mainCFG = getCFG(brboProgram.mainFunction)
    brboProgram.functions.foreach({
      function =>
        cfgs.get(function) match {
          case Some(_) =>
          case None => getCFG(function)
        }
    })
    ControlFlowGraph(mainCFG.root, cfgs, brboProgram, walaGraph, jgraphtGraph)
  }
}

// Every node is either a command or an expression
/**
 *
 * @param entryNode Root node of the CFG
 * @param cfgs A mapping from functions to their entries and exits in the CFG
 * @param brboProgram The program for which the CFG is generated
 * @param walaGraph A CFG that connects CFGs of all functions, in WALA representation
 * @param jgraphtGraph A CFG that connects CFGs of all functions, in jgrapht representation
 */
case class ControlFlowGraph(entryNode: CFGNode,
                            cfgs: Map[BrboFunction, InternalGraph],
                            brboProgram: BrboProgram,
                            walaGraph: NumberedGraph[CFGNode],
                            jgraphtGraph: SimpleDirectedGraph[CFGNode, DefaultEdge])
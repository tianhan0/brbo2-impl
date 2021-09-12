package brbo.common.cfg

import brbo.BrboMain
import brbo.common.ast._
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.impl.DelegatingNumberedGraph
import org.apache.logging.log4j.{LogManager, Logger}
import org.jgrapht.graph.{DefaultEdge, SimpleDirectedGraph}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.nio.{Attribute, AttributeType, DefaultAttribute}

import java.io.{File, IOException, PrintWriter, StringWriter}
import java.util
import scala.annotation.tailrec
import scala.collection.immutable.HashMap

case class InternalGraph(root: CFGNode, exits: Set[CFGNode])

object ControlFlowGraph {
  val OUTPUT_DIRECTORY: String = {
    val directory = s"${BrboMain.OUTPUT_DIRECTORY}/cfg"
    val file = new File(directory)
    file.mkdirs()
    directory
  }

  val logger: Logger = LogManager.getLogger(ControlFlowGraph.getClass.getName)

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
        case None =>
          // Create a node to represent an undefined function
          val node = getNode(Left(UndefinedFunction(functionName)), functionName)
          InternalGraph(node, Set(node))
      }
    }

    def getCFG(brboFunction: BrboFunction): InternalGraph = {
      val functionCFG = functionToInternalGraph(brboFunction)
      cfgs = cfgs + (brboFunction -> functionCFG)
      functionCFG
    }

    def getNode(content: Either[Command, BrboExpr], functionName: String): CFGNode = {
      nodes.get(content) match {
        case Some(node) => node
        case None =>
          val id = nodes.size + 1
          val node = CFGNode(content, functionName, id)
          nodes = nodes + (content -> node)
          addNode(node)
          node
      }
    }

    def functionToInternalGraph(brboFunction: BrboFunction): InternalGraph = {
      val exitNode = getNode(Left(FunctionExit()), brboFunction.identifier)
      val internalGraph = astToInternalGraph(brboFunction.body, JumpTarget(None, None, exitNode), brboFunction)
      internalGraph.exits.foreach(exit => if (exit != exitNode) addEdge(exit, exitNode))
      // Any function has exactly one exit node
      InternalGraph(internalGraph.root, Set(exitNode))
    }

    def astToInternalGraph(ast: BrboAst, jumpTarget: JumpTarget, brboFunction: BrboFunction): InternalGraph = {
      def addEdgesFromExitsToEntry(exits: Set[CFGNode], entry: CFGNode): Unit = {
        @tailrec
        def shouldAddEdge(nodeValue: Either[Command, BrboExpr]): Boolean = {
          nodeValue match {
            case Left(command) =>
              command match {
                case ReturnExpr(_, _) | ReturnVoid(_) | Break(_) | Continue(_) => false
                case LabeledCommand(_, command2, _) => shouldAddEdge(Left(command2))
                case Assert(_, _) | Assume(_, _) | VariableDeclaration(_, _, _) | Assignment(_, _, _) | Skip(_) | FunctionCall(_, _, _) => true
                case _: CFGOnly => true
              }
            case Right(_) => true
          }
        }

        exits.foreach({ exit => if (shouldAddEdge(exit.value)) addEdge(exit, entry) })
      }

      ast match {
        case command: Command =>
          val node = getNode(Left(command), brboFunction.identifier)
          addJumpEdges(command)

          @tailrec
          def addJumpEdges(command: Command): Unit = {
            command match {
              case Assert(_, _) | Assume(_, _) | VariableDeclaration(_, _, _) | Assignment(_, _, _) | Skip(_) =>
              case FunctionCall(_, functionCallExpr, _) =>
                val functionCFG = getCFGFromName(functionCallExpr.identifier)
                addEdge(node, functionCFG.root)
                functionCFG.exits.foreach(exit => addEdge(exit, node)) // TODO: When traversing, need to pair call and return edges.
              case ReturnExpr(_, _) | ReturnVoid(_) => addEdge(node, jumpTarget.functionExit)
              case Break(_) => addEdge(node, jumpTarget.immediateLoopExit.get)
              case Continue(_) => addEdge(node, jumpTarget.immediateLoopCondition.get)
              case LabeledCommand(_, command2, _) => addJumpEdges(command2)
              case _: CFGOnly => throw new Exception(s"`$command` is only used in Control Flow Graphs!")
            }
          }

          InternalGraph(node, Set(node))
        case statement: Statement =>
          statement match {
            case Block(statements, _) =>
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
            case ITE(condition, thenAst, elseAst, _) =>
              val conditionNode = getNode(Right(condition), brboFunction.identifier)
              addNode(conditionNode)

              val thenGraph = astToInternalGraph(thenAst, jumpTarget, brboFunction)
              val elseGraph = astToInternalGraph(elseAst, jumpTarget, brboFunction)

              addEdge(conditionNode, thenGraph.root)
              addEdge(conditionNode, elseGraph.root)

              InternalGraph(conditionNode, thenGraph.exits ++ elseGraph.exits)
            case Loop(condition, body, _) =>
              val conditionNode = getNode(Right(condition), brboFunction.identifier)
              val loopExit = getNode(Left(LoopExit()), brboFunction.identifier)
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

/**
 *
 * @param entryNode    Root node of the CFG
 * @param cfgs         A mapping from functions to their entries and exits in the CFG
 * @param brboProgram  The program for which the CFG is generated
 * @param walaGraph    A CFG that connects CFGs of all functions, in WALA representation
 * @param jgraphtGraph A CFG that connects CFGs of all functions, in jgrapht representation
 */
case class ControlFlowGraph(entryNode: CFGNode,
                            cfgs: Map[BrboFunction, InternalGraph],
                            brboProgram: BrboProgram,
                            walaGraph: NumberedGraph[CFGNode],
                            jgraphtGraph: SimpleDirectedGraph[CFGNode, DefaultEdge]) {
  def exportToDOT: String = {
    val exporter = new DOTExporter[CFGNode, DefaultEdge]
    val outputWriter = new StringWriter
    exporter.setVertexAttributeProvider({
      node: CFGNode =>
        val map = new util.HashMap[String, Attribute]()
        map.put("label", DefaultAttribute.createAttribute(node.toString))
        val shape: String =
          node.value match {
            case Left(command) =>
              command match {
                case _: CFGOnly => "oval"
                case _ => "rectangle"
              }
            case Right(_) => "diamond"
          }
        map.put("shape", new DefaultAttribute(shape, AttributeType.IDENTIFIER))
        map
    })
    exporter.exportGraph(jgraphtGraph, outputWriter)
    outputWriter.toString
  }

  def printPDF(): Unit = {
    val dotFilePath = s"${ControlFlowGraph.OUTPUT_DIRECTORY}/${brboProgram.name}.dot"
    val pdfFilePath = s"${ControlFlowGraph.OUTPUT_DIRECTORY}/${brboProgram.name}.pdf"

    val dotFileContents: String = exportToDOT
    val pw = new PrintWriter(new File(dotFilePath))
    pw.write(dotFileContents)
    pw.close()

    try {
      val command = "dot -Tpdf \"" + dotFilePath + "\" -o \"" + pdfFilePath + "\""
      val child = Runtime.getRuntime.exec(Array[String]("/bin/sh", "-c", command))
      child.waitFor
    } catch {
      case e@(_: InterruptedException | _: IOException) =>
        e.printStackTrace()
        System.exit(1)
    }
  }
}
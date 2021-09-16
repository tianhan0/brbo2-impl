package brbo.common.cfg

import brbo.BrboMain
import brbo.common.MathUtils
import brbo.common.TypeUtils.BrboType.BOOL
import brbo.common.ast._
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.impl.DelegatingNumberedGraph
import org.apache.logging.log4j.{LogManager, Logger}
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.graph.{DefaultEdge, SimpleDirectedWeightedGraph}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.nio.{Attribute, AttributeType, DefaultAttribute}

import java.io.{File, IOException, PrintWriter, StringWriter}
import java.util
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable

object ControlFlowGraph {
  val OUTPUT_DIRECTORY: String = {
    val directory = s"${BrboMain.OUTPUT_DIRECTORY}/cfg"
    val file = new File(directory)
    file.mkdirs()
    directory
  }

  val logger: Logger = LogManager.getLogger(ControlFlowGraph.getClass.getName)

  val TRUE_BRANCH_WEIGHT: Double = 1
  val FALSE_BRANCH_WEIGHT: Double = -1
  val DEFAULT_WEIGHT: Double = 0

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
    val jgraphtGraph = new SimpleDirectedWeightedGraph[CFGNode, DefaultEdge](classOf[DefaultEdge])

    def addNode(node: CFGNode): Unit = {
      walaGraph.addNode(node)
      jgraphtGraph.addVertex(node)
    }

    def addEdge(src: CFGNode, dst: CFGNode): Unit = {
      walaGraph.addEdge(src, dst)
      jgraphtGraph.addEdge(src, dst)
      jgraphtGraph.setEdgeWeight(src, dst, DEFAULT_WEIGHT)
    }

    def setEdgeWeight(src: CFGNode, dst: CFGNode, trueBranch: Boolean): Unit = {
      jgraphtGraph.setEdgeWeight(src, dst, if (trueBranch) TRUE_BRANCH_WEIGHT else FALSE_BRANCH_WEIGHT)
    }

    def getCFGFromName(functionName: String): InternalGraph = {
      (brboProgram.mainFunction :: brboProgram.functions).find(function => function.identifier == functionName) match {
        case Some(function) => getCFG(function)
        case None =>
          // Create a node to represent an undefined function
          val node = getNode(Left(UndefinedFunction(functionName)), ???)
          InternalGraph(node, Set(node))
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
                case Return(_, _) | Break(_) | Continue(_) => false
                case LabeledCommand(_, command2, _) => shouldAddEdge(Left(command2))
                case VariableDeclaration(_, _, _) | Assignment(_, _, _) | Skip(_) | FunctionCall(_, _, _) => true
                case _: CFGOnly => true
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
              case VariableDeclaration(_, _, _) =>
              case Assignment(_, _, _) =>
              case Skip(_) =>
              case FunctionCall(_, _, _) =>

              // No edge is added for function calls!
              case Return(_, _) => addEdge(node, jumpTarget.functionExit)
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
              val conditionNode = getNode(Right(condition), brboFunction)
              addNode(conditionNode)

              val thenGraph = astToInternalGraph(thenAst, jumpTarget, brboFunction)
              val elseGraph = astToInternalGraph(elseAst, jumpTarget, brboFunction)

              addEdge(conditionNode, thenGraph.root)
              setEdgeWeight(conditionNode, thenGraph.root, trueBranch = true)
              addEdge(conditionNode, elseGraph.root)
              setEdgeWeight(conditionNode, elseGraph.root, trueBranch = false)

              InternalGraph(conditionNode, thenGraph.exits ++ elseGraph.exits)
            case Loop(condition, body, _) =>
              val conditionNode = getNode(Right(condition), brboFunction)
              val loopExit = getNode(Left(LoopExit()), brboFunction)
              val bodyGraph = astToInternalGraph(body, JumpTarget(Some(conditionNode), Some(loopExit), jumpTarget.functionExit), brboFunction)

              addEdge(conditionNode, loopExit)
              setEdgeWeight(conditionNode, loopExit, trueBranch = false)
              addEdge(conditionNode, bodyGraph.root)
              setEdgeWeight(conditionNode, bodyGraph.root, trueBranch = true)
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
 * @param entryNode    Root node of the CFG. Every node is either a command or an expression.
 * @param cfgs         A mapping from functions to their entries and exits in the CFG
 * @param brboProgram  The program for which the CFG is generated
 * @param walaGraph    A CFG that connects CFGs of all functions, in WALA representation
 * @param jgraphtGraph A CFG that connects CFGs of all functions, in jgrapht representation
 */
case class ControlFlowGraph(entryNode: CFGNode,
                            cfgs: Map[BrboFunction, InternalGraph],
                            brboProgram: BrboProgram,
                            walaGraph: NumberedGraph[CFGNode],
                            jgraphtGraph: SimpleDirectedWeightedGraph[CFGNode, DefaultEdge]) {
  private val connectivityInspector = new ConnectivityInspector(jgraphtGraph)

  // Check if any two CFGs of two functions are disconnected
  MathUtils.crossJoin2(cfgs, cfgs).foreach({
    case (pair1, pair2) =>
      if (pair1 != pair2) {
        assert(!connectivityInspector.pathExists(pair1._2.root, pair2._2.root),
          s"CFG of function ${pair1._1.identifier} is connected with that of function ${pair2._1.identifier}")
      }
  })

  def exportToDOT: String = {
    val exporter = new DOTExporter[CFGNode, DefaultEdge]
    val outputWriter = new StringWriter
    exporter.setVertexAttributeProvider({
      node: CFGNode =>
        val map = new util.HashMap[String, Attribute]()
        map.put("label", DefaultAttribute.createAttribute(node.prettyPrintToCFG))
        val shape: String =
          node.value match {
            case Left(command) =>
              command match {
                case _: CFGOnly => "oval"
                case _ => "rectangle"
              }
            case Right(expr) =>
              assert(expr.typ == BOOL)
              "diamond"
          }
        map.put("shape", new DefaultAttribute(shape, AttributeType.IDENTIFIER))
        map
    })
    exporter.setEdgeAttributeProvider({
      edge =>
        val map = new util.HashMap[String, Attribute]()
        map.put("label", DefaultAttribute.createAttribute(jgraphtGraph.getEdgeWeight(edge)))
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

  def findSuccessorNodes(node: CFGNode): Set[CFGNode] = {
    val edges: mutable.Iterable[DefaultEdge] = jgraphtGraph.outgoingEdgesOf(node).asScala
    edges.map({ edge: DefaultEdge => jgraphtGraph.getEdgeTarget(edge) }).toSet
  }

  def findNextNodeNotSkipped(node: CFGNode): CFGNode = {
    def helper(node2: CFGNode): CFGNode = {
      val successorNodes = findSuccessorNodes(node2)
      successorNodes.size match {
        case 1 => findNextNodeNotSkipped(successorNodes.head)
        case _ => throw new Exception(s"Node `${node2.prettyPrintToC()}` must have 1 successor node!")
      }
    }

    node.value match {
      case Left(command) =>
        command match {
          case FunctionExit(_) => node // Not skip function exit
          case _: CFGOnly => helper(node) // Skip CFGOnly nodes, except for FunctionExit
          case Skip(_) => helper(node) // Skip the empty command
          case _ => node
        }
      case Right(_) => node
    }
  }

  def pathExists(src: CFGNode, dst: CFGNode): Boolean = connectivityInspector.pathExists(src, dst)
}
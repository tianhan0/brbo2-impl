package brbo.common.cfg

import brbo.BrboMain
import brbo.common.MathUtils
import brbo.common.ast._
import com.ibm.wala.util.graph.NumberedGraph
import com.ibm.wala.util.graph.dominators.DominanceFrontiers
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
    var nodes = new HashMap[Command, CFGNode]
    var cfgs = new HashMap[BrboFunction, InternalGraph]
    val walaGraph = new DelegatingNumberedGraph[CFGNode]()
    val jgraphtGraph = new SimpleDirectedWeightedGraph[CFGNode, DefaultEdge](classOf[DefaultEdge])

    def addEdge(src: CFGNode, dst: CFGNode): Unit = {
      walaGraph.addEdge(src, dst)
      jgraphtGraph.addEdge(src, dst)
      jgraphtGraph.setEdgeWeight(src, dst, DEFAULT_WEIGHT)
    }

    def setEdgeWeight(src: CFGNode, dst: CFGNode, trueBranch: Boolean): Unit = {
      jgraphtGraph.setEdgeWeight(src, dst, if (trueBranch) TRUE_BRANCH_WEIGHT else FALSE_BRANCH_WEIGHT)
    }

    /*def getCFGFromName(functionName: String): InternalGraph = {
      (brboProgram.mainFunction :: brboProgram.functions).find(function => function.identifier == functionName) match {
        case Some(function) => getCFG(function)
        case None =>
          // Create a node to represent an undefined function
          val node = getNode(Left(UndefinedFunction(functionName)), throw new Exception)
          InternalGraph(node, Set(node))
      }
    }*/

    def getCFG(brboFunction: BrboFunction): InternalGraph = {
      val functionCFG = functionToInternalGraph(brboFunction)
      cfgs = cfgs + (brboFunction -> functionCFG)
      functionCFG
    }

    def getNode(command: Command, brboFunction: BrboFunction): CFGNode = {
      def addNode(node: CFGNode): Unit = {
        walaGraph.addNode(node)
        jgraphtGraph.addVertex(node)
      }

      nodes.get(command) match {
        case Some(node) => node
        case None =>
          val id = nodes.size + 1
          val node = CFGNode(command, Some(brboFunction), id)
          nodes = nodes + (command -> node)
          addNode(node)
          node
      }
    }

    def functionToInternalGraph(brboFunction: BrboFunction): InternalGraph = {
      val exitNode = getNode(FunctionExit(), brboFunction)
      val internalGraph = astToInternalGraph(brboFunction.bodyWithInitialization, JumpTarget(None, None, exitNode), brboFunction)
      internalGraph.exits.foreach(exit => if (exit != exitNode) addEdge(exit, exitNode))
      // Any function has exactly one exit node
      InternalGraph(internalGraph.root, Set(exitNode))
    }

    def astToInternalGraph(ast: BrboAst, jumpTarget: JumpTarget, brboFunction: BrboFunction): InternalGraph = {
      def addEdgesFromExitsToEntry(exits: Set[CFGNode], entry: CFGNode): Unit = {
        @tailrec
        def shouldAddEdge(nodeValue: Command): Boolean = {
          nodeValue match {
            case _: BrboExpr => true
            case Return(_, _) | Break(_) | Continue(_) =>
              false // Not add edge for commands that do not respect the normal control flow
            case LabeledCommand(_, command2, _) => shouldAddEdge(command2)
            case VariableDeclaration(_, _, _) | Assignment(_, _, _) | Skip(_) | FunctionCallExpr(_, _, _, _) | Assume(_, _) => true
            case _: CFGOnly => true
            case _: GhostCommand => true
          }
        }

        exits.foreach({ exit => if (shouldAddEdge(exit.command)) addEdge(exit, entry) })
      }

      ast match {
        case command: Command =>
          val node = getNode(command, brboFunction)
          addJumpEdges(command)

          @tailrec
          def addJumpEdges(command: Command): Unit = {
            command match {
              case VariableDeclaration(_, _, _) =>
              case Assignment(_, _, _) =>
              case Skip(_) =>
              case FunctionCallExpr(_, _, _, _) => // No edge is added for function calls!
              case Assume(_, _) =>

              case Return(_, _) => addEdge(node, jumpTarget.functionExit)
              case Break(_) => addEdge(node, jumpTarget.immediateLoopExit.get)
              case Continue(_) => addEdge(node, jumpTarget.immediateLoopCondition.get)
              case LabeledCommand(_, command2, _) => addJumpEdges(command2)
              case _: CFGOnly => // throw new Exception(s"`$command` is only used in Control Flow Graphs!")
              case _: GhostCommand =>
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
              if (internalGraphs.isEmpty) {
                val emptyNode = getNode(Empty(), brboFunction)
                InternalGraph(emptyNode, Set(emptyNode))
              }
              else InternalGraph(internalGraphs.head.root, internalGraphs.last.exits)
            case ITE(condition, thenAst, elseAst, _) =>
              val branchNode = getNode(BranchingHead(), brboFunction)
              val conditionNode = getNode(condition, brboFunction)
              val negatedConditionNode = getNode(Negation(condition), brboFunction)

              val thenGraph = astToInternalGraph(thenAst, jumpTarget, brboFunction)
              val elseGraph = astToInternalGraph(elseAst, jumpTarget, brboFunction)

              addEdge(branchNode, conditionNode)
              setEdgeWeight(branchNode, conditionNode, trueBranch = true)
              addEdge(branchNode, negatedConditionNode)
              setEdgeWeight(branchNode, negatedConditionNode, trueBranch = false)

              addEdge(conditionNode, thenGraph.root)
              addEdge(negatedConditionNode, elseGraph.root)

              InternalGraph(branchNode, thenGraph.exits ++ elseGraph.exits)
            case Loop(condition, body, _) =>
              val branchNode = getNode(BranchingHead(), brboFunction)
              val conditionNode = getNode(condition, brboFunction)
              val negatedConditionNode = getNode(Negation(condition), brboFunction)
              val loopExit = getNode(LoopExit(), brboFunction)
              val bodyGraph = astToInternalGraph(body, JumpTarget(Some(branchNode), Some(loopExit), jumpTarget.functionExit), brboFunction)

              addEdge(branchNode, conditionNode)
              setEdgeWeight(branchNode, conditionNode, trueBranch = true)
              addEdge(branchNode, negatedConditionNode)
              setEdgeWeight(branchNode, negatedConditionNode, trueBranch = false)

              addEdge(conditionNode, bodyGraph.root)
              addEdge(negatedConditionNode, loopExit)
              addEdgesFromExitsToEntry(bodyGraph.exits, conditionNode)

              InternalGraph(branchNode, Set(loopExit))
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
 * @param brboProgram  The program from which the CFG is generated
 * @param walaGraph    A CFG that connects CFGs of all functions, in WALA representation
 * @param jgraphtGraph A CFG that connects CFGs of all functions, in jgrapht representation
 */
case class ControlFlowGraph(entryNode: CFGNode,
                            cfgs: Map[BrboFunction, InternalGraph],
                            brboProgram: BrboProgram,
                            walaGraph: NumberedGraph[CFGNode],
                            jgraphtGraph: SimpleDirectedWeightedGraph[CFGNode, DefaultEdge]) {
  private val connectivityInspector = new ConnectivityInspector(jgraphtGraph)
  private val dominanceFrontiers = new DominanceFrontiers(walaGraph, entryNode)

  // Ensure CFGs of any two functions are disconnected
  MathUtils.crossJoin2(cfgs, cfgs).foreach({
    case (pair1, pair2) =>
      if (pair1 != pair2) {
        assert(!pathExists(pair1._2.root, pair2._2.root),
          s"CFG of function ${pair1._1.identifier} is connected with CFG of function ${pair2._1.identifier}")
      }
  })

  def closestDominator(nodes: Set[CFGNode], predicate: CFGNode => Boolean): Option[CFGNode] = {
    closestDominator(root = entryNode, nodes, predicate, visited = Set())
  }

  private def closestDominator(root: CFGNode, nodes: Set[CFGNode], predicate: CFGNode => Boolean, visited: Set[CFGNode]): Option[CFGNode] = {
    val isRootDominator = nodes.forall(node => dominanceFrontiers.isDominatedBy(node, root))
    if (!isRootDominator) return None
    val qualifyingSuccessors: Iterator[CFGNode] = walaGraph.getSuccNodes(root).asScala.flatMap({
      successor =>
        if (visited.contains(successor)) None
        else closestDominator(root = successor, nodes, predicate, visited = visited + root)
    })
    if (qualifyingSuccessors.isEmpty) Some(root).filter(predicate)
    else Some(qualifyingSuccessors.toList.head)
  }

  def nodesFromCommands(commands: Set[Command]): Set[CFGNode] = {
    jgraphtGraph.vertexSet().asScala.foldLeft(Set[CFGNode]())({
      case (acc, node) =>
        if (commands.contains(node.command)) acc + node
        else acc
    })
  }

  def exportToDOT: String = {
    val exporter = new DOTExporter[CFGNode, DefaultEdge]
    val outputWriter = new StringWriter
    exporter.setVertexAttributeProvider({
      node: CFGNode =>
        val map = new util.HashMap[String, Attribute]()
        map.put("label", DefaultAttribute.createAttribute(node.printToIR()))
        val shape: String =
          node.command match {
            case _: BrboExpr => "diamond"
            case _: CFGOnly => "oval"
            case _ => "rectangle"
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

  def successorNodes(node: CFGNode): Set[CFGNode] = {
    val edges: mutable.Iterable[DefaultEdge] = jgraphtGraph.outgoingEdgesOf(node).asScala
    edges.map({ edge: DefaultEdge => jgraphtGraph.getEdgeTarget(edge) }).toSet
  }

  def pathExists(src: CFGNode, dst: CFGNode): Boolean = connectivityInspector.pathExists(src, dst)
}
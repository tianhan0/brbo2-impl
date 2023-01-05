package brbo.common.cfg

import brbo.BrboMain
import brbo.common.MathUtils
import brbo.common.ast._
import com.ibm.wala.util.graph.dominators.DominanceFrontiers
import com.ibm.wala.util.graph.impl.DelegatingNumberedGraph
import com.ibm.wala.util.graph.{INodeWithNumberedEdges, NumberedGraph}
import org.apache.logging.log4j.{LogManager, Logger}
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.graph.{DefaultEdge, SimpleDirectedGraph, SimpleDirectedWeightedGraph}
import org.jgrapht.nio.dot.DOTExporter
import org.jgrapht.nio.{Attribute, AttributeType, DefaultAttribute}

import java.io.{File, IOException, PrintWriter, StringWriter}
import java.util
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.{HashMap, HashSet}
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

  case class JumpTarget(immediateLoopBranchHead: Option[CFGNode], immediateLoopExit: Option[CFGNode], functionExit: CFGNode)

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

    def generateCFG(brboFunction: BrboFunction): InternalGraph = {
      val functionCFG = functionToInternalGraph(brboFunction)
      cfgs = cfgs + (brboFunction -> functionCFG)
      functionCFG
    }

    def generateNode(command: Command, brboFunction: BrboFunction): CFGNode = {
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
      val exitNode = generateNode(FunctionExit(), brboFunction)
      val jumpTarget = JumpTarget(immediateLoopBranchHead = None, immediateLoopExit = None, functionExit = exitNode)
      val internalGraph = astToInternalGraph(
        ast = brboFunction.bodyWithGhostInitialization,
        jumpTarget = jumpTarget,
        brboFunction = brboFunction
      )
      internalGraph.exits.foreach(exit => addEdge(exit, exitNode))
      // Any function has exactly one exit node
      InternalGraph(internalGraph.root, Set(exitNode))
    }

    @tailrec
    def shouldAddEdgeToEntryOfNextStatement(nodeValue: Command): Boolean = {
      nodeValue match {
        case _: BrboExpr => true
        case Return(_, _) | Break(_) | Continue(_) =>
          false // Not add edge for commands that do not respect the normal control flow
        case LabeledCommand(_, command2, _) => shouldAddEdgeToEntryOfNextStatement(command2)
        case VariableDeclaration(_, _, _) | Assignment(_, _, _) | Skip(_) |
             FunctionCallExpr(_, _, _, _) | Assume(_, _) | Comment(_, _) => true
        case _: CFGOnly => true
        case _: GhostCommand => true
      }
    }

    def addEdgesFromExitsToEntry(exits: Set[CFGNode], entry: CFGNode): Unit =
      exits.foreach({ exit => if (shouldAddEdgeToEntryOfNextStatement(exit.command)) addEdge(exit, entry) })

    @tailrec
    def addJumpEdges(node: CFGNode, command: Command, jumpTarget: JumpTarget): Unit = {
      command match {
        case VariableDeclaration(_, _, _) =>
        case Assignment(_, _, _) =>
        case Skip(_) | Comment(_, _) =>
        case FunctionCallExpr(_, _, _, _) => // No edge is added for function calls!
        case Assume(_, _) =>

        case Return(_, _) => addEdge(node, jumpTarget.functionExit)
        case Break(_) => addEdge(node, jumpTarget.immediateLoopExit.get)
        case Continue(_) => addEdge(node, jumpTarget.immediateLoopBranchHead.get)
        case LabeledCommand(_, command, _) => addJumpEdges(node, command, jumpTarget)
        case _: CFGOnly => // throw new Exception(s"`$command` is only used in Control Flow Graphs!")
        case _: GhostCommand =>
      }
    }

    def astToInternalGraph(ast: BrboAst, jumpTarget: JumpTarget, brboFunction: BrboFunction): InternalGraph = {
      ast match {
        case command: Command =>
          val node = generateNode(command, brboFunction)
          addJumpEdges(node, command, jumpTarget)
          InternalGraph(node, Set(node))
        case statement: Statement =>
          statement match {
            case Block(statements, _) =>
              val internalGraphs = statements.map(statement => astToInternalGraph(statement, jumpTarget, brboFunction))
              internalGraphs.size match {
                case 0 =>
                  val emptyNode = generateNode(Empty(), brboFunction)
                  InternalGraph(emptyNode, Set(emptyNode))
                case 1 =>
                  InternalGraph(internalGraphs.head.root, internalGraphs.head.exits)
                case _ =>
                  var i = 1
                  while (i < internalGraphs.size) {
                    val previous = internalGraphs(i - 1)
                    val current = internalGraphs(i)
                    addEdgesFromExitsToEntry(previous.exits, current.root)
                    i = i + 1
                  }
                  InternalGraph(internalGraphs.head.root, internalGraphs.last.exits)
              }
            case ITE(condition, thenAst, elseAst, _) =>
              val branchNode = generateNode(BranchingHead(), brboFunction)
              val conditionNode = generateNode(condition, brboFunction)
              val negatedConditionNode = generateNode(Negation(condition), brboFunction)

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
              val branchNode = generateNode(BranchingHead(), brboFunction)
              val conditionNode = generateNode(condition, brboFunction)
              val negatedConditionNode = generateNode(Negation(condition), brboFunction)
              val loopExit = generateNode(LoopExit(), brboFunction)
              val bodyGraph = astToInternalGraph(
                body,
                JumpTarget(
                  immediateLoopBranchHead = Some(branchNode),
                  immediateLoopExit = Some(loopExit),
                  functionExit = jumpTarget.functionExit
                ),
                brboFunction
              )

              addEdge(branchNode, conditionNode)
              setEdgeWeight(src = branchNode, dst = conditionNode, trueBranch = true)
              addEdge(branchNode, negatedConditionNode)
              setEdgeWeight(src = branchNode, dst = negatedConditionNode, trueBranch = false)

              addEdge(conditionNode, bodyGraph.root)
              addEdge(negatedConditionNode, loopExit)
              addEdgesFromExitsToEntry(bodyGraph.exits, branchNode)

              InternalGraph(branchNode, Set(loopExit))
          }
      }
    }

    val mainCFG = generateCFG(brboProgram.mainFunction)
    brboProgram.otherFunctions.foreach({
      function =>
        cfgs.get(function) match {
          case Some(_) =>
          case None => generateCFG(function)
        }
    })
    ControlFlowGraph(mainCFG.root, cfgs, brboProgram, nodes, walaGraph, jgraphtGraph)
  }

  def compareNodes[Node](left: Node, right: Node): Boolean = left.toString < right.toString

  def deepCopyNumberedGraph[Node <: INodeWithNumberedEdges](graph: NumberedGraph[Node],
                                                            entry: Node,
                                                            exit: Node,
                                                            copyNode: Node => Node,
                                                            reverse: Boolean): (NumberedGraph[Node], Node) = {
    def successorNodes(node: Node): List[Node] = graph.getSuccNodes(node).asScala.toList.sortWith(compareNodes)

    def addNode(graph: NumberedGraph[Node], node: Node): Unit = graph.addNode(node)

    def addEdge(graph: NumberedGraph[Node], from: Node, to: Node): Unit = graph.addEdge(from, to)

    val newGraph = new DelegatingNumberedGraph[Node]()
    val root = deepCopyGraph[Node, NumberedGraph[Node]](
      newGraph = newGraph,
      successorNodes = successorNodes,
      addNode = addNode,
      addEdge = addEdge,
      copyNode = copyNode,
      entry = entry,
      exit = exit,
      reverse = reverse
    )
    (newGraph, root)
  }

  def deepCopySimpleDirectedGraph[Node](graph: SimpleDirectedGraph[Node, DefaultEdge],
                                        entry: Node,
                                        exit: Node,
                                        copyNode: Node => Node,
                                        reverse: Boolean): (SimpleDirectedGraph[Node, DefaultEdge], Node) = {
    def successorNodes(node: Node): List[Node] = ControlFlowGraph.successorNodesSorted(graph, node, compareNodes)

    def addNode(graph: SimpleDirectedGraph[Node, DefaultEdge], node: Node): Unit = graph.addVertex(node)

    def addEdge(graph: SimpleDirectedGraph[Node, DefaultEdge], from: Node, to: Node): Unit = graph.addEdge(from, to)

    val newGraph = new SimpleDirectedGraph[Node, DefaultEdge](classOf[DefaultEdge])
    val root = deepCopyGraph[Node, SimpleDirectedGraph[Node, DefaultEdge]](
      newGraph = newGraph,
      successorNodes = successorNodes,
      addNode = addNode,
      addEdge = addEdge,
      copyNode = copyNode,
      entry = entry,
      exit = exit,
      reverse = reverse
    )
    (newGraph, root)
  }

  private def deepCopyGraph[Node, Graph](newGraph: Graph,
                                         successorNodes: Node => List[Node],
                                         addNode: (Graph, Node) => Unit,
                                         addEdge: (Graph, Node, Node) => Unit,
                                         copyNode: Node => Node,
                                         entry: Node,
                                         exit: Node,
                                         reverse: Boolean): Node = {
    // A mapping from nodes to their copies in the new graph
    var nodes = Map[Node, Node]()
    var visited = HashSet[Node]()
    val stack = new java.util.Stack[Node]
    stack.push(entry)

    def generateCopy(node: Node): Node = {
      nodes.get(node) match {
        case Some(value) => value
        case None =>
          val copied = copyNode(node)
          nodes = nodes + (node -> copied)
          copied
      }
    }

    while (!stack.empty()) {
      val top: Node = stack.pop()
      if (!visited.contains(top)) {
        visited = visited + top
        // Add the node upon the 1st visit
        addNode(newGraph, generateCopy(top))
      }
      successorNodes(top).foreach({
        successorNode =>
          if (!visited.contains(successorNode))
            stack.push(successorNode)
          // Add the reversed edge
          val from = top
          val to = successorNode
          val copiedFrom = generateCopy(from)
          val copiedTo = generateCopy(to)
          addNode(newGraph, copiedFrom)
          addNode(newGraph, copiedTo)
          if (reverse) addEdge(newGraph, copiedTo, copiedFrom)
          else addEdge(newGraph, copiedFrom, copiedTo)
      })
    }
    if (reverse) generateCopy(exit) else generateCopy(entry)
  }

  def successorNodes[T](graph: SimpleDirectedGraph[T, DefaultEdge], node: T): Iterator[T] = {
    val edges: mutable.Iterable[DefaultEdge] = graph.outgoingEdgesOf(node).asScala
    edges.map({ edge: DefaultEdge => graph.getEdgeTarget(edge) }).toIterator
  }

  def successorNodesSorted[T](graph: SimpleDirectedGraph[T, DefaultEdge],
                              node: T,
                              compare: (T, T) => Boolean): List[T] = successorNodes(graph, node).toList.sortWith(compare)

  def printDotToPDF(filename: String, dotFileContents: String): Unit = {
    val dotFilePath = s"${ControlFlowGraph.OUTPUT_DIRECTORY}/$filename.dot"
    val pdfFilePath = s"${ControlFlowGraph.OUTPUT_DIRECTORY}/$filename.pdf"

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

  def exportToDOT(graph: SimpleDirectedGraph[CFGNode, DefaultEdge]): String = {
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
        map.put("shape", new DefaultAttribute(shape, AttributeType.STRING))
        map
    })
    exporter.setEdgeAttributeProvider({
      edge =>
        val map = new util.HashMap[String, Attribute]()
        map.put("label", DefaultAttribute.createAttribute(graph.getEdgeWeight(edge)))
        map
    })
    exporter.exportGraph(graph, outputWriter)
    outputWriter.toString
  }

  // Find the node in the subgraph (rooted at `entryNode` of `graph`) that
  // 1. dominates all `nodes`,
  // 2. satisfies `predicate`, and
  // 3. is the "closest". That is, there exists no other node that is strictly dominated by this node, but also satisfies the above two conditions.
  def closestDominator[T <: INodeWithNumberedEdges](graph: NumberedGraph[T],
                                                    entryNode: T,
                                                    nodes: Set[T],
                                                    predicate: T => Boolean): Option[T] = {
    val dominanceFrontiers = new DominanceFrontiers(graph, entryNode)
    closestDominatorInternal(graph, dominanceFrontiers, root = entryNode, nodes, predicate, visited = Set())
  }

  // Find the node in the subgraph (rooted at `root` of `graph`) that satisfies the conditions above
  private def closestDominatorInternal[T](graph: NumberedGraph[T],
                                          dominanceFrontiers: DominanceFrontiers[T],
                                          root: T,
                                          nodes: Set[T],
                                          predicate: T => Boolean,
                                          visited: Set[T]): Option[T] = {
    val isRootDominator = nodes.forall(node => dominanceFrontiers.isDominatedBy(node, root))
    if (!isRootDominator) return None
    val candidates: Iterator[T] = graph.getSuccNodes(root).asScala.flatMap({
      successor =>
        if (visited.contains(successor)) None
        else closestDominatorInternal(graph, dominanceFrontiers, root = successor, nodes, predicate, visited = visited + root)
    })
    if (candidates.isEmpty) Some(root).filter(predicate)
    else Some(candidates.toList.head)
  }

  def reverseGraph(controlFlowGraph: ControlFlowGraph): (NumberedGraph[CFGNode], CFGNode) = {
    val (entry, exit) = {
      val graph = controlFlowGraph.cfgs(controlFlowGraph.brboProgram.mainFunction)
      (graph.root, graph.exits.head)
    }

    ControlFlowGraph.deepCopyNumberedGraph(
      graph = controlFlowGraph.walaGraph,
      entry = entry,
      exit = exit,
      copyNode = CFGNode.copyNodeOnly,
      reverse = true
    )
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
                            nodes: Map[Command, CFGNode],
                            walaGraph: NumberedGraph[CFGNode],
                            jgraphtGraph: SimpleDirectedWeightedGraph[CFGNode, DefaultEdge]) {
  private val connectivityInspector = new ConnectivityInspector(jgraphtGraph)

  // Ensure CFGs of any two functions are disconnected
  MathUtils.crossJoin2(cfgs, cfgs).foreach({
    case (pair1, pair2) =>
      if (pair1 != pair2) {
        assert(!pathExists(pair1._2.root, pair2._2.root),
          s"CFG of function ${pair1._1.identifier} is connected with CFG of function ${pair2._1.identifier}")
      }
  })

  def closestDominator(nodes: Set[CFGNode], predicate: CFGNode => Boolean): Option[CFGNode] =
    ControlFlowGraph.closestDominator[CFGNode](graph = walaGraph, entryNode, nodes, predicate)

  def nodesFromCommands(commands: Set[Command]): Set[CFGNode] = {
    nodes.filter({ case (command, _) => commands.contains(command) })
      .map({ case (_, node) => node })
      .toSet
  }

  def printPDF(): Unit =
    ControlFlowGraph.printDotToPDF(filename = brboProgram.className, dotFileContents = ControlFlowGraph.exportToDOT(jgraphtGraph))

  def successorNodes(node: CFGNode): Iterator[CFGNode] = ControlFlowGraph.successorNodes(jgraphtGraph, node)

  def pathExists(src: CFGNode, dst: CFGNode): Boolean = connectivityInspector.pathExists(src, dst)
}
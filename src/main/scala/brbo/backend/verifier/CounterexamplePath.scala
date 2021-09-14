package brbo.backend.verifier

import brbo.common.TypeUtils.BrboType.BOOL
import brbo.common.ast._
import brbo.common.cfg.{CFGNode, ControlFlowGraph}
import org.apache.logging.log4j.LogManager
import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.builder.GraphTypeBuilder
import org.jgrapht.nio.graphml.GraphMLImporter
import org.jgrapht.nio.{Attribute, ImportException}
import org.jgrapht.traverse.TopologicalOrderIterator
import org.jgrapht.util.SupplierUtil

import java.util.function.Supplier
import scala.collection.JavaConverters._
import scala.collection.mutable

case class CounterexamplePath(nodes: List[(BrboFunction, CFGNode)]) extends PrettyPrintToC {
  nodes.map(node => node._2.value).foreach({
    case Left(_) =>
    case Right(expr) => assert(expr.typ == BOOL)
  })

  override def prettyPrintToC(indent: Int): String = {
    nodes.map({
      case (function, node) => s"${node.prettyPrintToC()} [Function `${function.identifier}`]"
    }).mkString("\n")
  }
}

object CounterexamplePath {
  private val logger = LogManager.getLogger("brbo.backend.verifier.CounterexamplePath")

  def graphMLToCounterexamplePath(graphMLString: String, brboProgram: BrboProgram): CounterexamplePath = {
    val vertexAttributes = new mutable.HashMap[String, mutable.HashMap[String, Attribute]]
    val edgeAttributes = new mutable.HashMap[DefaultEdge, mutable.HashMap[String, Attribute]]
    val graph = readGraph(new java.io.StringReader(graphMLString), classOf[DefaultEdge], directed = true, weighted = false, vertexAttributes, edgeAttributes)

    var path: List[String] = Nil
    var lastVertex: Option[String] = None
    val iterator = new TopologicalOrderIterator(graph)
    while (iterator.hasNext) {
      val current = iterator.next()
      lastVertex match {
        case Some(last) =>
          // Get the edge between the last and the current nodes
          val edges = graph.getAllEdges(last, current).asScala
          assert(edges.size == 1)

          edgeAttributes.get(edges.head) match {
            case Some(attributes) =>
              attributes.get("sourcecode") match {
                case Some(sourcecode) => path = sourcecode.toString :: path
                case None => throw new Exception
              }
            case None =>
          }
        case None =>
      }
      lastVertex = Some(current)
    }
    path = path.reverse
    logger.error(s"Counterexample path (as a list of strings):\n${path.mkString("\n")}")

    val counterexamplePath = parsePathString(path, brboProgram)
    logger.error(s"Counterexample path (as a list of CFG nodes):\n${counterexamplePath.prettyPrintToC()}")
    assert(counterexamplePath.nodes.nonEmpty)
    counterexamplePath
  }

  case class MatchResult(matched: Boolean, matchedExpression: Boolean, matchedTrueBranch: Boolean)

  // TODO: Parse pathNode into an AST for comparison, unless we can assume UAutomizer outputs commands and expressions in a same syntax as brbo2
  private def matchNode(pathNode: String, node: CFGNode): MatchResult = {
    val exactString =
      if (pathNode.startsWith("[") && pathNode.endsWith("]")) pathNode.substring(1, pathNode.length - 1)
      else pathNode
    node.value match {
      case Left(_) => MatchResult(node.prettyPrintToC() == exactString, matchedExpression = false, matchedTrueBranch = false)
      case Right(_) =>
        val negated = s"!(${node.prettyPrintToC()})"
        logger.error(s"Negated: `$negated`. Exact string: `$exactString`.")
        val matchedTrueBranch = node.prettyPrintToC() == exactString
        val matchedFalseBranch = negated == exactString
        MatchResult(matchedTrueBranch || matchedFalseBranch, matchedExpression = true, matchedTrueBranch)
    }
  }

  /**
   *
   * @param path        A counterexample path, which is a list of strings
   * @param brboProgram The function to parse the counterexample path against
   * @return Parse strings into BrboAst or BrboExpr by matching against the pretty print strings of the entry function
   */
  private def parsePathString(path: List[String], brboProgram: BrboProgram): CounterexamplePath = {
    val cfg = ControlFlowGraph.toControlFlowGraph(brboProgram)

    /*def findMatchedNodes(pathNode: String, nodesToMatch: Set[CFGNode]): Set[CFGNode] = {
      val exactString =
        if (pathNode.startsWith("[") && pathNode.endsWith("]")) pathNode.substring(1, pathNode.length - 2)
        else pathNode
      nodesToMatch.filter(node => matchNode(exactString, node))
    }*/

    /**
     * @param currentNode     The node to be matched
     * @param currentFunction The function that the current node belongs to
     * @param callStack       The current call stack. The head is the immediate caller of the current function,
     *                        together with the next node in this caller that needs to be matched (or, the return target)
     * @param matchedNodes    The sequence of CFG nodes that have been matched
     * @param remainingPath   The remaining path (which is a string) to be matched
     * @param shouldContinue  Whether to continue to match nodes, if starting from this state
     */
    case class State(currentNode: CFGNode, currentFunction: BrboFunction, callStack: List[(BrboFunction, CFGNode)],
                     matchedNodes: List[(BrboFunction, CFGNode)], remainingPath: List[String], shouldContinue: Boolean) {
      override def toString: String = {
        val s1 = s"Current node: `${currentNode.prettyPrintToC()}`"
        val s2 = s"Current function: `${currentFunction.identifier}`"
        val s3 = s"Call stack: `${callStack.map(pair => s"`${pair._2.prettyPrintToC()}` in function `${pair._1.identifier}`")}"
        val s4 = s"Matched nodes: `${matchedNodes.map({ pair => s"`${pair._2.prettyPrintToC()}`" }).reverse}`"
        val s5 = s"Remaining path: `$remainingPath`"
        val s6 = s"Should continue: `$shouldContinue`"
        List(s1, s2, s3, s4, s5, s6).mkString("\n")
      }
    }

    /**
     *
     * Traverse the CFG with breadth first search, starting from the given node, until the newly explored sequence of nodes no longer matches the path
     *
     * @param state The current state in the traversal
     * @return The next state in the traversal
     */
    def bfs(state: State): State = {
      logger.error(s"Current state: `$state`")
      val notContinueState = State(state.currentNode, state.currentFunction, Nil, Nil, Nil, shouldContinue = false)
      if (!state.shouldContinue) return notContinueState

      // First, match against the sequence of function calls in the current node
      val functionCalls: List[FunctionCallExpr] = state.currentNode.getFunctionCalls
      logger.error(s"Found function calls in `${state.currentNode}`: `$functionCalls`")
      val functions: List[(CFGNode, BrboFunction)] =
        functionCalls.flatMap({
          functionCall =>
            cfg.cfgs.find({ case (function, _) => function.identifier == functionCall.identifier }) match {
              case Some(value) => Some((value._2.root, value._1))
              case None =>
                logger.trace(s"Calling a function that has no CFG: `${functionCall.identifier}`")
                None
            }
        })
      val newState: State = {
        var newState = state
        var i = 0
        while (i < functions.size && newState.shouldContinue) {
          val (functionEntry, function) = functions(i)
          logger.error(s"Match function call: `${function.identifier}`")
          newState = bfs(State(functionEntry, function, (state.currentFunction, state.currentNode) :: state.callStack,
            newState.matchedNodes, newState.remainingPath, newState.shouldContinue))
          i = i + 1
        }
        newState
      }

      if (!newState.shouldContinue) notContinueState
      else {
        assert(newState.callStack == state.callStack)
        newState.remainingPath match {
          case Nil => newState
          case ::(head, tail) =>
            val matchResult = matchNode(head, state.currentNode)
            logger.error(s"Current node `${state.currentNode}` ${if (matchResult.matched) "does" else "does not"} match path node `$head`")
            if (!matchResult.matched) {
              notContinueState
            }
            else {
              val successorNode: Option[CFGNode] = {
                val successorNodes: Option[CFGNode] = {
                  val successorNodes = cfg.findSuccessorNodes(state.currentNode)
                  assert(successorNodes.size <= 2)
                  if (matchResult.matchedExpression) {
                    assert(successorNodes.size == 2)
                    // Find the branch to proceed
                    val (trueNode: CFGNode, falseNode: CFGNode) = {
                      val n1 = successorNodes.head
                      val n2 = successorNodes.tail.head
                      val e1 = cfg.jgraphtGraph.getEdge(state.currentNode, n1)
                      val e2 = cfg.jgraphtGraph.getEdge(state.currentNode, n2)
                      (cfg.jgraphtGraph.getEdgeWeight(e1), cfg.jgraphtGraph.getEdgeWeight(e2)) match {
                        case (ControlFlowGraph.TRUE_BRANCH_WEIGHT, ControlFlowGraph.FALSE_BRANCH_WEIGHT) => (n1, n2)
                        case (ControlFlowGraph.FALSE_BRANCH_WEIGHT, ControlFlowGraph.TRUE_BRANCH_WEIGHT) => (n2, n1)
                        case _ => throw new Exception
                      }
                    }
                    val theNodeToProcess = if (matchResult.matchedTrueBranch) trueNode else falseNode
                    cfg.findNextNodeNotSkipped(theNodeToProcess)
                  }
                  else {
                    successorNodes.size match {
                      case 0 => None
                      case 1 => cfg.findNextNodeNotSkipped(successorNodes.head)
                    }
                  }
                }
                logger.error(s"Successor node (non-CFGOnly): `$successorNodes`")

                if (successorNodes.nonEmpty) successorNodes // A non-empty set of new nodes to match
                else { // There is no successor node
                  if (tail.isEmpty) successorNodes // There is no more path nodes to parse
                  else {
                    state.currentNode.value match {
                      case Left(command) =>
                        logger.error(s"Current node `$command` has no successor nodes")
                        assert(command.isInstanceOf[Return], s"Command $command is not a return")
                        assert(state.callStack.nonEmpty)
                        logger.error(s"Let the successor node be the return target `${state.callStack.head._2}`")
                        Some(state.callStack.head._2)
                      case Right(_) => throw new Exception(s"Cannot find non-CFGOnly successor nodes for node `${state.currentNode}`")
                    }
                  }
                }
              }

              successorNode match {
                case Some(successorNode2) =>
                  val newState2 = bfs(State(successorNode2, state.currentFunction, state.callStack,
                    (state.currentFunction, state.currentNode) :: newState.matchedNodes, tail, shouldContinue = true))
                  assert(newState2.callStack == state.callStack)
                  // Only keep new states that allow continuing to match
                  if (newState2.shouldContinue) newState2
                  else notContinueState
                case None => notContinueState
              }
            }
        }
      }
    }

    // Match the head of the path against all possible next nodes
    /*def helper(path: List[String], nodesToMatch: Set[CFGNode], lastMatchedNode: Option[CFGNode], lastMatchedNodesInCaller: List[CFGNode]): Option[CounterexamplePath] = {
      nodesToMatch.foreach({
        node =>
          node.value match {
            case Left(command) => assert(!command.isInstanceOf[CFGOnly]) // CFGOnly nodes must be properly skipped during the match
            case Right(_) =>
          }
      })
      logger.error(s"path: $path")
      logger.error(s"nodesToMatch: $nodesToMatch")
      logger.error(s"lastMatchedNode: $lastMatchedNode")
      logger.error(s"lastMatchedNodesInCaller: $lastMatchedNodesInCaller")

      path match {
        case Nil => Some(CounterexamplePath(Nil))
        case ::(head, tail) =>
          val matchedNodes: Set[CFGNode] = findMatchedNodes(head, nodesToMatch ++ functionEntryNodes)
          val allCounterexamplePaths: Set[(CFGNode, CounterexamplePath)] =
            matchedNodes.flatMap({
              matchedNode =>
                val startAnotherFunction: Boolean =
                  lastMatchedNode match {
                    case Some(lastMatchedNode2) =>
                      // Assume any two CFGs of two functions are disconnected
                      logger.error(s"$lastMatchedNode2, $matchedNode, ${cfg.pathExists(lastMatchedNode2, matchedNode)}")
                      if (cfg.pathExists(lastMatchedNode2, matchedNode)) false // Continue to match in the current function
                      else true // Match in another function
                    case None => false // No node has been matched (because we have just started)
                  }

                val newNodesToMatch: Set[CFGNode] = {
                  val nodesToMatchInCurrentFunction: Set[CFGNode] = cfg.findSuccessorNodesNonCFGOnly(matchedNode) // Skip CFGOnly nodes
                  if (nodesToMatchInCurrentFunction.isEmpty) { // There is no successor node
                    if (tail.nonEmpty) {
                      matchedNode.value match {
                        case Left(command) =>
                          if (command.isInstanceOf[Return]) {
                            val lastMatchedNodeInCaller: CFGNode =
                              if (startAnotherFunction) lastMatchedNode.get // TODO
                              else lastMatchedNodesInCaller.head // TODO

                            val edges2 = cfg.jgraphtGraph.outgoingEdgesOf(lastMatchedNodeInCaller).asScala
                            edges2.map({ edge: DefaultEdge => cfg.jgraphtGraph.getEdgeTarget(edge) }).toSet
                          }
                          else throw new Exception(s"CFG node `$matchedNode` is matched with `$head`, but it does not have any successor nodes`!")
                        case Right(_) => throw new Exception(s"CFG node `$matchedNode` is matched with `$head`, but it does not have any successor nodes`!")
                      }
                    }
                    else nodesToMatchInCurrentFunction // There is no more path nodes to parse
                  }
                  else nodesToMatchInCurrentFunction // A non-empty set of new nodes to match
                }

                val newLastMatchedNodesInCaller = {
                  if (startAnotherFunction) matchedNode :: lastMatchedNodesInCaller // Match in another function
                  else lastMatchedNodesInCaller // Continue to match in the current function
                }

                helper(tail, newNodesToMatch, Some(matchedNode), newLastMatchedNodesInCaller) match {
                  case Some(counterexamplePath) => Some(matchedNode, counterexamplePath)
                  case None => None
                }
            })
          allCounterexamplePaths.size match {
            case 0 => None
            case 1 =>
              val counterexamplePath = allCounterexamplePaths.head
              Some(CounterexamplePath(counterexamplePath._1 :: counterexamplePath._2.nodes))
            case _ =>
              val newline = "\n"
              throw new Exception(s"Found multiple counterexample paths:\n${allCounterexamplePaths.mkString(newline)}")
          }
      }
    }

    helper(path, Set(cfg.entryNode), None, Nil)*/

    val entryNode = cfg.cfgs(brboProgram.mainFunction).root
    val state = bfs(State(entryNode, brboProgram.mainFunction, Nil, Nil, path, shouldContinue = true))
    logger.error(s"State after matching: $state")
    assert(state.shouldContinue)
    CounterexamplePath(state.matchedNodes.reverse)
  }

  // Copied from https://github.com/jgrapht/jgrapht/blob/6aba8e81053660997fe681c50974c07e312027d1/jgrapht-io/src/test/java/org/jgrapht/nio/graphml/GraphMLImporterTest.java
  @throws[ImportException]
  private def readGraph[E](input: java.io.StringReader, edgeClass: Class[E],
                           directed: Boolean, weighted: Boolean,
                           vertexAttributes: mutable.HashMap[String, mutable.HashMap[String, Attribute]],
                           edgeAttributes: mutable.HashMap[E, mutable.HashMap[String, Attribute]]): Graph[String, E] = {
    val stringSupplier: Supplier[String] = SupplierUtil.createStringSupplier(1)
    val g: Graph[String, E] =
      if (directed) GraphTypeBuilder.directed[String, E].allowingMultipleEdges(true).allowingSelfLoops(true).weighted(weighted).vertexSupplier(stringSupplier).edgeClass(edgeClass).buildGraph
      else GraphTypeBuilder.undirected[String, E].allowingMultipleEdges(true).allowingSelfLoops(true).weighted(weighted).vertexSupplier(stringSupplier).edgeClass(edgeClass).buildGraph

    val importer = new GraphMLImporter[String, E]
    importer.addVertexAttributeConsumer({
      case (k: org.jgrapht.alg.util.Pair[String, String], a: Attribute) =>
        val vertex = k.getFirst
        val attrs =
          vertexAttributes.get(vertex) match {
            case Some(attrs) => attrs
            case None =>
              val attrs = new mutable.HashMap[String, Attribute]
              vertexAttributes.put(vertex, attrs)
              attrs
          }
        attrs.put(k.getSecond, a)
    })

    importer.addEdgeAttributeConsumer({
      case (k: org.jgrapht.alg.util.Pair[E, String], a: Attribute) =>
        val edge = k.getFirst
        val attrs =
          edgeAttributes.get(edge) match {
            case Some(attrs) => attrs
            case None =>
              val attrs = new mutable.HashMap[String, Attribute]
              edgeAttributes.put(edge, attrs)
              attrs
          }
        attrs.put(k.getSecond, a)
    })

    importer.importGraph(g, input)
    g
  }
}
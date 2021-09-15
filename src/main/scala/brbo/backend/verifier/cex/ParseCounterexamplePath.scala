package brbo.backend.verifier.cex

import brbo.backend.verifier.cex
import brbo.common.MyLogger
import brbo.common.TypeUtils.BrboType.VOID
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
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable

class ParseCounterexamplePath(debugMode: Boolean) {
  private val logger = MyLogger.createLogger(classOf[ParseCounterexamplePath], debugMode)

  def graphMLToCounterexamplePath(graphMLString: String, brboProgram: BrboProgram): Path = {
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
    logger.intoOrError(s"Counterexample path (as a list of strings):\n${path.mkString("\n")}")

    val counterexamplePath = parsePathString(path, brboProgram)
    logger.intoOrError(s"Counterexample path (as a list of CFG nodes):\n${counterexamplePath.prettyPrintToC()}")
    assert(counterexamplePath.pathNodes.nonEmpty)
    counterexamplePath
  }

  /**
   *
   * @param path        A counterexample path, which is a list of strings
   * @param brboProgram The function to parse the counterexample path against
   * @return Parse strings into BrboAst or BrboExpr by matching against the pretty print strings of the entry function
   */
  private def parsePathString(path: List[String], brboProgram: BrboProgram): Path = {
    val cfg = ControlFlowGraph.toControlFlowGraph(brboProgram)

    val fakeNode = CFGNode(Left(Skip()), "???", 0)
    val fakeFunction = BrboFunction("???", VOID, Nil, Block(List(Skip())))

    /**
     *
     * Traverse the CFG with breadth first search, starting from the given node, until the newly explored sequence of nodes no longer matches the path
     *
     * @param state The current state in the traversal
     * @return The state where we need to stop the traversal
     */
    def matchPath(state: State): State = {
      logger.traceOrError(s"Current state:\n$state")
      if (!state.shouldContinue) return state

      val currentNode = state.subState.currentNode
      val currentFunction = state.subState.currentFunction

      // Pop from the call stack
      currentNode.value match {
        case Left(command) =>
          command match {
            case FunctionExit(_) | Return(None, _) => // These commands need not to be matched, since they don't appear in UAutomizer's outputs
              if (state.callStack.isEmpty) {
                if (state.remainingPath.nonEmpty)
                  throw new Exception(s"Exiting function `${currentFunction.identifier}`, " +
                    s"but the call stack is empty and the remaining path is not empty: `${state.remainingPath}`")
                return State(SubState(fakeNode, processFunctionCalls = true, fakeFunction), Nil, state.matchedNodes, Nil, shouldContinue = false)
              }
              else {
                val top = state.callStack.head
                logger.traceOrError(s"Return to `$top`")
                return matchPath(State(top, state.callStack.tail, state.matchedNodes, state.remainingPath, state.shouldContinue))
              }
            case _ =>
          }
        case Right(_) =>
      }

      val newState: State = {
        if (!state.subState.processFunctionCalls) {
          logger.traceOrError(s"Not process function calls in node `$currentNode`")
          state
        }
        else {
          // First, match against the sequence of function calls in the current node
          val functionCalls: List[FunctionCallExpr] = currentNode.getFunctionCalls
          if (functionCalls.nonEmpty) logger.traceOrError(s"Found function calls in `$currentNode`: `$functionCalls`")
          val functions: List[(CFGNode, BrboFunction)] =
            functionCalls.flatMap({
              functionCall =>
                cfg.cfgs.find({ case (function, _) => function.identifier == functionCall.identifier }) match {
                  case Some(value) => Some((value._2.root, value._1))
                  case None =>
                    logger.traceOrError(s"Calling a function that has no CFG: `${functionCall.identifier}`")
                    None
                }
            })
          var newState = state
          var i = 0
          while (i < functions.size && newState.shouldContinue) {
            val (functionEntry, function) = functions(i)
            logger.traceOrError(s"Calling function: `${function.identifier}`")
            val (returnTarget, processFunctionCalls) = {
              val skipCurrentNodeWhenReturn: Boolean = {
                currentNode.value match {
                  case Left(command) =>
                    command match {
                      case FunctionCall(variable, _, _) =>
                        variable match {
                          case Some(_) => false
                          case None => true // Skip function calls whose return values are not assigned to any variable
                        }
                      case _ => false
                    }
                  case Right(_) => false
                }
              }
              if (skipCurrentNodeWhenReturn) {
                val successorNodes = cfg.findSuccessorNodes(currentNode)
                assert(successorNodes.size == 1)
                (cfg.findNextNodeNotSkipped(successorNodes.head), true) // If skipping the current node, then must process function calls for the next node
              }
              else (currentNode, false) // If not skipping the current node, then there is no need to process the function calls again
            }
            logger.traceOrError(s"Set the return target: `$returnTarget` in function `${currentFunction.identifier}`. Process function calls: $processFunctionCalls.")
            newState = matchPath(
              State(
                SubState(functionEntry, processFunctionCalls = true, function),
                SubState(returnTarget, processFunctionCalls, currentFunction) :: state.callStack, // Push into the call stack
                newState.matchedNodes, newState.remainingPath, newState.shouldContinue
              )
            )
            i = i + 1
          }
          newState
        }
      }

      if (!newState.shouldContinue) newState
      else {
        assert(newState.callStack == state.callStack,
          s"New call stack: ${newState.callStack}.\n Old call stack: ${state.callStack}.")
        assert(currentNode == newState.subState.currentNode,
          s"Current node: `$currentNode`. Node from new state: `${newState.subState.currentNode}`")
        assert(currentFunction == newState.subState.currentFunction,
          s"Current function: `${currentFunction.identifier}`. Function from new state: `${newState.subState.currentFunction.identifier}`")

        newState.remainingPath match {
          case Nil => throw new Exception(s"Match node `$currentNode` with an empty path")
          case ::(head, tail) =>
            val matchResult = matchNode(head, currentNode)
            logger.traceOrError(s"Current node `$currentNode` ${if (matchResult.matched) "indeed matches" else "does not match"} path node `$head`")
            if (!matchResult.matched) newState
            else {
              val newCurrentNode: CFGNode = {
                val successorNode: CFGNode = {
                  val successorNodes = cfg.findSuccessorNodes(currentNode)
                  if (matchResult.matchedExpression) {
                    // Matched an expression
                    assert(successorNodes.size == 2)
                    // Find the branch to proceed
                    val (trueNode: CFGNode, falseNode: CFGNode) = {
                      val n1 = successorNodes.head
                      val n2 = successorNodes.tail.head
                      val e1 = cfg.jgraphtGraph.getEdge(currentNode, n1)
                      val e2 = cfg.jgraphtGraph.getEdge(currentNode, n2)
                      (cfg.jgraphtGraph.getEdgeWeight(e1), cfg.jgraphtGraph.getEdgeWeight(e2)) match {
                        case (ControlFlowGraph.TRUE_BRANCH_WEIGHT, ControlFlowGraph.FALSE_BRANCH_WEIGHT) => (n1, n2)
                        case (ControlFlowGraph.FALSE_BRANCH_WEIGHT, ControlFlowGraph.TRUE_BRANCH_WEIGHT) => (n2, n1)
                        case _ => throw new Exception
                      }
                    }
                    if (matchResult.matchedTrueBranch) trueNode else falseNode
                  }
                  else {
                    // Matched a command
                    assert(successorNodes.size == 1)
                    successorNodes.head
                  }
                }
                logger.traceOrError(s"Successor node: `$successorNode`")
                cfg.findNextNodeNotSkipped(successorNode)
              }
              logger.traceOrError(s"Successor node (non-CFGOnly): `$newCurrentNode`")

              // The new call stack might be different from the current call stack, because it might have processed function returns
              // Every call to this function is responsible for maintaining the call stack in order for a potential recursive call to itself to work properly
              matchPath(State(SubState(newCurrentNode, processFunctionCalls = true, currentFunction), newState.callStack,
                cex.PathNode(currentNode, currentFunction) :: newState.matchedNodes, tail, newState.shouldContinue))
            }
        }
      }
    }

    val entryNode = cfg.cfgs(brboProgram.mainFunction).root
    val state = matchPath(State(SubState(entryNode, processFunctionCalls = true, brboProgram.mainFunction), Nil, Nil, path, shouldContinue = true))
    logger.traceOrError(s"State after matching:\n$state")
    assert(state.subState.currentNode == fakeNode)
    assert(state.subState.currentFunction == fakeFunction)
    assert(!state.shouldContinue)
    Path(state.matchedNodes.reverse)
  }

  private case class MatchResult(matched: Boolean, matchedExpression: Boolean, matchedTrueBranch: Boolean)

  // In order to get an AST node from a string, we probably do not need to parse pathNode into an AST,
  // because the input to UAutomizer is always the result of prettyPrintToC(), which we compare against with the result of invoking prettyPrintToC() on AST nodes
  private def matchNode(pathNode: String, node: CFGNode): MatchResult = {
    val exactString =
      if (pathNode.startsWith("[") && pathNode.endsWith("]")) pathNode.substring(1, pathNode.length - 1)
      else pathNode
    node.value match {
      case Left(command) =>
        val expected = expectedUAutomizerString(command)
        logger.traceOrError(s"Expected UAutomizer string: `$expected` (`${command.getClass}`)")
        MatchResult(expected == exactString, matchedExpression = false, matchedTrueBranch = false)
      case Right(brboExpr) =>
        val expected = brboExpr.prettyPrintToCNoOuterBrackets
        val negated = s"!($expected)"
        logger.traceOrError(s"Expected UAutomizer string: `$expected`. Negated: `$negated`. Exact string: `$exactString`.")
        val matchedTrueBranch = expected == exactString
        val matchedFalseBranch = negated == exactString
        MatchResult(matchedTrueBranch || matchedFalseBranch, matchedExpression = true, matchedTrueBranch)
    }
  }

  @tailrec
  private def expectedUAutomizerString(command: Command): String = {
    def getRidOfSemicolon(command2: Command): String = {
      val s = command2.prettyPrintToC()
      assert(s.endsWith(";"))
      s.substring(0, s.length - 1)
    }

    command match {
      case Assignment(_, _, _) | FunctionCall(_, _, _) => getRidOfSemicolon(command)
      case LabeledCommand(_, command2, _) => expectedUAutomizerString(command2)
      case _ => command.prettyPrintToC()
    }
  }

  /**
   * @param subState       The current substate
   * @param callStack      The current call stack. The head is the immediate caller of the current function,
   *                       together with the next node in this caller that needs to be matched (or, the return target)
   * @param matchedNodes   The sequence of CFG nodes that have been matched
   * @param remainingPath  The remaining path (which is a string) to be matched
   * @param shouldContinue Whether to continue to match nodes, if starting from this state
   */
  private case class State(subState: SubState,
                           callStack: List[SubState],
                           matchedNodes: List[PathNode],
                           remainingPath: List[String],
                           shouldContinue: Boolean) {
    override def toString: String = {
      val s1 = s"Current node: `${subState.currentNode.prettyPrintToC()}`"
      val s2 = s"Current function: `${subState.currentFunction.identifier}`"
      val s3 = s"Process function calls: `${subState.processFunctionCalls}`"
      val s4 = s"Call stack: `${callStack.map({ subState => subState.toString })}`"
      val s5 = s"Matched nodes: `${matchedNodes.map({ pathNode => s"`${pathNode.node.prettyPrintToC()}`" }).reverse}`"
      val s6 = s"Remaining path: `$remainingPath`"
      val s7 = s"Should continue: `$shouldContinue`"
      List(s1, s2, s3, s4, s5, s6, s7).mkString("\n")
    }
  }

  /**
   *
   * @param currentNode          The node to be matched
   * @param processFunctionCalls Whether to process the function calls in the current node
   * @param currentFunction      The function that the current node belongs to
   */
  private case class SubState(currentNode: CFGNode, processFunctionCalls: Boolean, currentFunction: BrboFunction) {
    override def toString: String = s"Node ${currentNode.prettyPrintToC()} in function `${currentFunction.identifier}` (Process function calls: $processFunctionCalls)"
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
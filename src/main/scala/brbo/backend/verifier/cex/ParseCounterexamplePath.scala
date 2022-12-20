package brbo.backend.verifier.cex

import brbo.common.BrboType.VOID
import brbo.common.MyLogger
import brbo.common.ast._
import brbo.common.cfg.{CFGNode, ControlFlowGraph}
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
    logger.infoOrError(s"Counterexample path (as a list of strings):\n${path.mkString("\n")}")

    val counterexamplePath = parsePathString(path, brboProgram)
    logger.infoOrError(s"Counterexample path (as a list of CFG nodes):\n${counterexamplePath.toString()}")
    assert(counterexamplePath.pathNodes.nonEmpty)
    counterexamplePath
  }

  // Map updates to ghost variables into use or reset commands.
  // Assume the input program contains no use or reset commands.
  def extractUseResetFromCRepresentation(path: Path, programInC: BrboCProgram): Path = {
    def extractUseReset(key: Command, node: CFGNode): CFGNode = {
      programInC.map.get(key) match {
        case Some(value) =>
          logger.traceOrError(s"Translate `$key` (in C representation) to `${value.asInstanceOf[Command].printToIR}`")
          CFGNode(value.asInstanceOf[Command], node.function, CFGNode.DONT_CARE_ID)
        case None => node
      }
    }

    val newNodes: List[CFGNode] = path.pathNodes.map({
      node =>
        node.command match {
          case _: Command => extractUseReset(node.command, node)
          case brboExpr: BrboExpr =>
            brboExpr match {
              case Negation(notNegated, _) =>
                // Properly extract a use / reset when its conditionals are negated
                extractUseReset(notNegated, node)
              case _ => extractUseReset(node.command, node)
            }
        }
    })
    var result: List[CFGNode] = Nil
    var lastNode: Option[CFGNode] = None
    newNodes.indices.foreach({
      i =>
        // Get rid of repetitive use / reset commands, because a use / reset is translated to multiple commands or expressions in a C program
        lastNode match {
          case Some(value) =>
            if (value != newNodes(i))
              result = newNodes(i) :: result
          case None => result = newNodes(i) :: result
        }
        lastNode = Some(newNodes(i))
    })
    Path(result.reverse)
  }

  /**
   *
   * @param path        A counterexample path, which is a list of strings
   * @param brboProgram The function to parse the counterexample path against
   * @return Parse strings into BrboAst or BrboExpr by matching against the pretty print strings of the entry function
   */
  private def parsePathString(path: List[String], brboProgram: BrboProgram): Path = {
    val cfg = ControlFlowGraph.toControlFlowGraph(brboProgram)
    if (debugMode) cfg.printPDF()

    val subStateWhenMatchSucceed = {
      val functionWhenMatchSucceed = BrboFunction("!!!", VOID, Nil, Block(List(Skip())), Set())
      val nodeWhenMatchSucceed = CFGNode(Skip())
      SubState(nodeWhenMatchSucceed, processFunctionCalls = true, functionWhenMatchSucceed)
    }

    /**
     *
     * Traverse the CFG with breadth first search, starting from the given node, until the newly explored sequence of nodes no longer matches the path
     *
     * @param state The current state in the traversal
     * @return The state where we need to stop the traversal
     */
    def matchPath(state: State): State = {
      logger.traceOrError(s"Current state:\n$state")
      if (!state.shouldContinue)
        throw new Exception
      if (state.remainingPath.isEmpty && state.callStack.isEmpty) {
        logger.traceOrError(s"Stop matching, because the remaining path and the call stack are both empty!")
        return State(subStateWhenMatchSucceed, Nil, state.matchedNodes, Nil, shouldContinue = false)
      }

      val currentNode = state.subState.currentNode
      val currentFunction = state.subState.currentFunction

      // Early return for commands that cannot be matched (since they don't appear in UAutomizer's outputs)
      currentNode.command match {
        case command: Command =>
          command match {
            case FunctionExit(_) | Return(None, _) =>
              logger.traceOrError(s"Pop from the call stack")
              if (state.callStack.isEmpty) {
                if (state.remainingPath.nonEmpty)
                  throw new Exception(s"Exiting function `${currentFunction.identifier}`, " +
                    s"but the call stack is empty and the remaining path is not empty: `${state.remainingPath}`")
                return State(subStateWhenMatchSucceed, Nil, state.matchedNodes, Nil, shouldContinue = false)
              }
              else {
                val top = state.callStack.head
                logger.traceOrError(s"Return to `$top`")
                // Put the current node into the list of matched notes, so that function calls and returns will match in the path
                val appendCurrentNode: Boolean =
                  state.matchedNodes match {
                    case Nil => true
                    case ::(head, _) =>
                      head.command match {
                        case command2: Command =>
                          command2 match {
                            case Return(_, _) =>
                              assert(command.isInstanceOf[FunctionExit])
                              false // Avoid duplicate function returns
                            case _ => true
                          }
                        case _: BrboExpr => true
                      }
                  }
                val matchedNodes = if (appendCurrentNode) currentNode :: state.matchedNodes else state.matchedNodes
                return matchPath(State(top, state.callStack.tail, matchedNodes, state.remainingPath, state.shouldContinue))
              }
            case _: CFGOnly | Skip(_) =>
              logger.traceOrError(s"Skip the empty command and CFGOnly nodes (except for FunctionExit): `$currentNode`")
              val successorNodes = cfg.successorNodes(currentNode)
              successorNodes.size match {
                case 1 =>
                  val newSubState = SubState(successorNodes.next(), state.subState.processFunctionCalls, state.subState.currentFunction)
                  return matchPath(State(newSubState, state.callStack, state.matchedNodes, state.remainingPath, state.shouldContinue))
                case _ => throw new Exception(s"Node `${currentNode.printToIR()}` must have 1 successor node!")
              }
            case _ =>
          }
        case _: BrboExpr =>
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
          val functions: List[(CFGNode, BrboFunction, List[BrboExpr])] =
            functionCalls.flatMap({
              functionCall =>
                cfg.cfgs.find({ case (function, _) => function.identifier == functionCall.identifier }) match {
                  case Some(value) => Some((value._2.root, value._1, functionCall.arguments))
                  case None =>
                    logger.traceOrError(s"Calling a function that has no CFG: `${functionCall.identifier}`")
                    None
                }
            })
          var newState = state
          var i = 0
          while (i < functions.size && newState.shouldContinue) {
            val (functionEntry, function, actualArguments) = functions(i)
            logger.traceOrError(s"Calling function: `${function.identifier}`")
            val (returnTarget, processFunctionCalls) = {
              val skipCurrentNodeWhenReturn: Boolean = {
                currentNode.command match {
                  case command: Command =>
                    command match {
                      case FunctionCallExpr(_, _, _, _) => true // Skip function calls whose return values are not assigned to any variable
                      case _ => false
                    }
                  case _: BrboExpr => false
                }
              }
              if (skipCurrentNodeWhenReturn) {
                val successorNodes = cfg.successorNodes(currentNode)
                assert(successorNodes.size == 1)
                (successorNodes.next(), true) // If skipping the current node, then must process function calls for the next node
              }
              else (currentNode, false) // If not skipping the current node, then there is no need to process the function calls again
            }
            logger.traceOrError(s"Set the return target: `$returnTarget` in function `${currentFunction.identifier}`. Process function calls: $processFunctionCalls.")
            logger.traceOrError(s"Push the current function into the call stack: `${currentFunction.identifier}`")
            newState = matchPath(
              State(
                SubState(functionEntry, processFunctionCalls = true, function),
                SubState(returnTarget, processFunctionCalls, currentFunction) :: state.callStack,
                // Insert a special command, indicating the immediate next commands are from the called function
                CFGNode(BeforeFunctionCall(function, actualArguments), Some(currentFunction), CFGNode.DONT_CARE_ID) :: newState.matchedNodes,
                newState.remainingPath,
                newState.shouldContinue
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
            val (matchResult, rematchCurrentPathNode) = {
              val result: MatchResult = matchNode(head, currentNode)
              logger.traceOrError(s"Current AST node `$currentNode` ${if (result.matched) "indeed matches" else "does not match"} current path node `$head`")
              if (!result.matched) {
                currentNode.command match {
                  case command: Command =>
                    command match {
                      case _@(Continue(_) | Break(_)) =>
                        logger.traceOrError(s"Decide to match control flow AST node `$currentNode` with no path node.")
                        logger.traceOrError(s"Will re-match current path node `$head`.")
                        (MatchResult(matched = true, matchedExpression = false, matchedTrueBranch = false), true)
                      case _ => throw new Exception
                    }
                  case _: BrboExpr =>
                    // Decide to match anyway, since constant bool expressions never show up in a path
                    // This will always match the true branch, because otherwise this node will appear in the trace (and thus we won't end up here)
                    logger.traceOrError(s"Decide to match conditional AST node `$currentNode` with no path node.")
                    logger.traceOrError(s"Will re-match current path node `$head`.")
                    (MatchResult(matched = true, matchedExpression = true, matchedTrueBranch = true), true)
                }
              }
              else (result, false)
            }
            val newCurrentNode: CFGNode = {
              val successorNodes = cfg.successorNodes(currentNode)
              if (matchResult.matchedExpression) {
                // Matched an expression
                assert(successorNodes.size == 2, s"Successor nodes:\n`${successorNodes.map(n => n.printToIR()).mkString("\n")}`")
                // Find the branch to proceed
                val (trueNode: CFGNode, falseNode: CFGNode) = {
                  val n1 = successorNodes.next()
                  val n2 = successorNodes.next()
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
                successorNodes.next()
              }
            }
            logger.traceOrError(s"Successor node: `$newCurrentNode`")

            val matchedNode: CFGNode = {
              if (!matchResult.matchedExpression) currentNode // Match a command
              else {
                currentNode.command match {
                  case _: Command => throw new Exception
                  case brboExpr: BrboExpr =>
                    if (matchResult.matchedTrueBranch) currentNode // Match the true branch
                    else CFGNode(Negation(brboExpr), currentNode.function, currentNode.id) // Match the false branch
                }
              }
            }

            val newRemainingPath = if (rematchCurrentPathNode) newState.remainingPath else tail
            // The new call stack might be different from the current call stack, because it might have processed function returns
            // Every call to this function is responsible for maintaining the call stack in order for a potential recursive call to itself to work properly
            matchPath(State(SubState(newCurrentNode, processFunctionCalls = true, currentFunction), newState.callStack,
              matchedNode :: newState.matchedNodes, newRemainingPath, newState.shouldContinue))
        }
      }
    }

    val entryNode = cfg.cfgs(brboProgram.mainFunction).root
    val state = matchPath(State(SubState(entryNode, processFunctionCalls = true, brboProgram.mainFunction), Nil, Nil, path, shouldContinue = true))
    logger.traceOrError(s"State after matching:\n$state")
    assert(state.subState == subStateWhenMatchSucceed)
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
    node.command match {
      case command: Command =>
        val expected = expectedUAutomizerString(command)
        logger.traceOrError(s"Expected UAutomizer string: `$expected` (`${command.getClass}`)")
        MatchResult(expected == exactString, matchedExpression = false, matchedTrueBranch = false)
      case brboExpr: BrboExpr =>
        val expected = brboExpr.printNoOuterBrackets
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
      val s = command2.printToC(0)
      assert(s.endsWith(";"))
      s.substring(0, s.length - 1)
    }

    command match {
      case Assignment(_, _, _) => getRidOfSemicolon(command)
      case LabeledCommand(_, command2, _) => expectedUAutomizerString(command2)
      case _ => command.printToC(0)
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
                           matchedNodes: List[CFGNode],
                           remainingPath: List[String],
                           shouldContinue: Boolean) {
    override def toString: String = {
      val s1 = s"Current node: `${subState.currentNode.printToIR}`"
      val s2 = s"Current function: `${subState.currentFunction.identifier}`"
      val s3 = s"Process function calls: `${subState.processFunctionCalls}`"
      val s4 = s"Call stack: `${callStack.map({ subState => subState.toString })}`"
      val s5 = s"Matched nodes: `${matchedNodes.map({ pathNode => s"`${pathNode.printToIR}`" }).reverse}`"
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
    override def toString: String = s"Node ${currentNode.printToIR()} in function `${currentFunction.identifier}` (Process function calls: $processFunctionCalls)"
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
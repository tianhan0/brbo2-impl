package brbo.backend.driver

import brbo.BrboMain
import brbo.backend.driver.NodeStatus._
import brbo.backend.refiner.{Refinement, Refiner}
import brbo.backend.verifier.VerifierStatus._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.modelchecker.AbstractMachine
import brbo.backend.verifier.{InterpreterKind, SymbolicExecution, UAutomizerVerifier, VerifierResult}
import brbo.common._
import brbo.common.ast._
import brbo.common.string.StringFormatUtils
import org.apache.commons.io.FileUtils
import org.jgrapht.graph.{DefaultEdge, SimpleDirectedGraph}

import java.io.File
import java.nio.charset.Charset
import scala.annotation.tailrec
import scala.collection.JavaConverters._

class Driver(arguments: CommandLineArguments, originalProgram: BrboProgram) {
  private val logger = MyLogger.createLogger(classOf[Driver], arguments.getDebugMode)
  private val uAutomizerVerifier = new UAutomizerVerifier(arguments)
  private val afterExtractingUses: BrboProgram = extractUsesFromMain(originalProgram)

  private val initialAbstractionGroupId = 0

  def verify(boundAssertion: BoundAssertion): Unit = {
    arguments.getAmortizationMode match {
      case brbo.backend.verifier.AmortizationMode.NO_AMORTIZE =>
        logger.info("Mode: Worst-case reasoning [1/1]")
        verifyWorstCase(boundAssertion)
      case brbo.backend.verifier.AmortizationMode.FULL_AMORTIZE =>
        logger.info("Mode: Fully-amortized reasoning [1/1]")
        verifyFullyAmortize(boundAssertion)
      case brbo.backend.verifier.AmortizationMode.SELECTIVE_AMORTIZE =>
        logger.info("Mode: Selectively-amortized reasoning [1/1]")
        verifySelectivelyAmortize(boundAssertion)
      case brbo.backend.verifier.AmortizationMode.ALL_AMORTIZE =>
        logger.info("Mode: Worst-case reasoning [1/3]")
        verifyWorstCase(boundAssertion)
        logger.info("Mode: Fully-amortized reasoning [2/3]")
        verifyFullyAmortize(boundAssertion)
        logger.info("Mode: Selectively-amortized reasoning [3/3]")
        verifySelectivelyAmortize(boundAssertion)
      case brbo.backend.verifier.AmortizationMode.TEST_MODE =>
        logger.info("Unknown mode. Exiting.")
        sys.exit(-1)
    }
  }

  def verifyFullyAmortize(boundAssertion: BoundAssertion): VerifierResult = {
    ???
  }

  def verifyWorstCase(boundAssertion: BoundAssertion): VerifierResult = {
    ???
  }

  def validateCexPath(path: Path): Boolean = {
    // TODO: Validate if the false result is spurious
    false
  }

  def verifySelectivelyAmortize(boundAssertion: BoundAssertion): VerifierStatus = {
    val initialAbstraction = generateInitialAbstraction(afterExtractingUses)
    val result = verify(initialAbstraction, boundAssertion)
    val counterexamplePaths: Set[Path] = result.rawResult match {
      case TRUE_RESULT =>
        logger.infoOrError(s"Verifier successfully verifies the initial abstraction!")
        return TRUE_RESULT
      case FALSE_RESULT =>
        logger.infoOrError(s"Verifier fails to verify the initial abstraction!")
        result.counterexamplePaths
      case UNKNOWN_RESULT =>
        logger.infoOrError(s"Verifier returns `$UNKNOWN_RESULT` for the initial abstraction.")
        result.counterexamplePaths
    }
    val existsTrueCexPath = counterexamplePaths.exists(p => validateCexPath(p))
    if (existsTrueCexPath) return FALSE_RESULT

    // No Self-loops; No Multiple Edges; No Weighted
    val tree = new SimpleDirectedGraph[TreeNode, DefaultEdge](classOf[DefaultEdge])
    val root = createNode(tree, parent = None, initialAbstraction, counterexamplePaths, refinement = None)
    val refiner = new Refiner(arguments)

    @tailrec
    def helper(node: TreeNode): VerifierStatus = {
      logger.infoOrError(s"Explored `${tree.vertexSet().size()}` variations of the input program. " +
        s"Will stop at `${arguments.getMaxIterations + 1}`.")
      if (tree.vertexSet().size() > arguments.getMaxIterations) {
        logger.infoOrError(s"Output all unknown amortizations to `${BrboMain.OUTPUT_DIRECTORY}/amortizations/`")
        tree.vertexSet().asScala.zipWithIndex.foreach({
          case (node, index) =>
            val programInC = BrboCProgram(node.program)
            val cSourceCode = programInC.program.printToC(0)
            val file = new File(s"${BrboMain.OUTPUT_DIRECTORY}/amortizations/${originalProgram.className}-${StringFormatUtils.integer(index, 3)}.txt")
            FileUtils.writeStringToFile(file, cSourceCode, Charset.forName("UTF-8"))
        })
        logger.infoOrError(s"Reached the max number of refinement iterations: `${arguments.getMaxIterations}`. Will stop now.")
        return UNKNOWN_RESULT
      }
      // Avoid refinements / child nodes that should not be explored
      val avoidRefinements: Set[Refinement] = {
        tree.outgoingEdgesOf(node).asScala.foldLeft(Set[Refinement]())({
          (acc, edge) =>
            val childNode = tree.getEdgeTarget(edge)
            childNode.getNodeStatus match {
              case Some(status) =>
                status match {
                  case EXPLORING => acc
                  case SHOULD_NOT_EXPLORE => acc + childNode.refinement.get
                }
              case None => acc
            }
        })
      }
      if (result.counterexamplePaths.isEmpty) {
        logger.infoOrError(s"Will backtrack because no counterexample is provided for the current node.")
        getParentNode(tree, node) match {
          case Some(parent) => helper(parent)
          case None =>
            logger.infoOrError(s"No parent node to backtrack to.")
            UNKNOWN_RESULT
        }
      }
      else {
        // Assume that all assert() in node.program are ub checks, so that we can safely remove all assert() from the cex.
        // TODO: Should make use of all counterexample paths
        // TODO: Randomly select a counterexample path to refine the failed program
        val pathWithoutUBChecks = result.counterexamplePaths.head // Path.removeCommandsForUBCheck(result.counterexamplePaths.head)
        // Refine the current node, possibly once again, based on the same counterexample
        // When a node is visited for a second (or more) time, then it must be caused by backtracking, which
        // implies that all nodes in one of its subtree have failed
        refiner.refine(node.program, pathWithoutUBChecks, boundAssertion, avoidRefinements) match {
          case (Some(refinedProgram), Some(refinement)) =>
            val result = verify(refinedProgram, boundAssertion)
            val newChildNode = createNode(tree, parent = Some(node), refinedProgram, result.counterexamplePaths, Some(refinement))
            val exploreRefinedProgram: NodeStatus = result.rawResult match {
              case TRUE_RESULT | FALSE_RESULT => EXPLORING
              case UNKNOWN_RESULT => SHOULD_NOT_EXPLORE
            }
            newChildNode.setNodeStatus(exploreRefinedProgram)

            result.rawResult match {
              case TRUE_RESULT =>
                logger.infoOrError(s"Successfully verified the child of the current node!")
                TRUE_RESULT
              case FALSE_RESULT =>
                logger.infoOrError(s"Explore child nodes of the child of the current node (which has failed).")
                val existsTrueCexPath = result.counterexamplePaths.exists(path => validateCexPath(path))
                if (existsTrueCexPath) return FALSE_RESULT
                helper(newChildNode)
              case UNKNOWN_RESULT =>
                logger.infoOrError(s"Explore a new child node of the current node (because the verification returns unknown) by re-refining the current node.")
                val existsTrueCexPath = result.counterexamplePaths.exists(path => validateCexPath(path))
                if (existsTrueCexPath) return FALSE_RESULT
                helper(node)
            }
          case (None, None) =>
            getParentNode(tree, node) match {
              case Some(parent) => helper(parent)
              case None =>
                logger.infoOrError(s"No parent node to backtrack to.")
                UNKNOWN_RESULT
            }
          case _ => throw new Exception
        }
      }
    }

    helper(root)
  }

  private def verify(program: BrboProgram, boundAssertion: BoundAssertion): VerifierResult = {
    val assertion = boundAssertion.replaceResourceVariable(program.mainFunction.approximatedResourceUsage)
    logger.infoOrError(s"Verify global assertion `${assertion.printNoOuterBrackets}`")
    // val ubcheckInserted = insertUBCheck(program, boundAssertion)
    // val result = uAutomizerVerifier.verify(ubcheckInserted)
    val modelChecker = new AbstractMachine(program, arguments)
    val result = modelChecker.verify(assertion, None).result
	// TODO: Check the validity of the counterexample path if refuted?
    logger.infoOrError(s"Verifier result: `$result`.")
    result
  }

  private def extractUsesFromMain(program: BrboProgram): BrboProgram = {
    val allCommands = BrboAstUtils.collectCommands(program.mainFunction.body)
    val (replacements, _) = allCommands.foldLeft((Map[Command, Command](), Map[String, Int]()))({
      case ((replacements, ids), command) =>
        command match {
          case Assignment(variable, expression, _) =>
            if (GhostVariableUtils.isGhostVariable(variable.name, GhostVariableTyp.Resource)) {
              val errorMessage = s"To successfully extract uses from assignments, the assignment must be in the form of " +
                s"`${variable.name} = ${variable.name} + e` for some e, instead of `${command.printToC(0)}`"
              expression match {
                case Addition(left, right, _) =>
                  left match {
                    case identifier@Identifier(variableName, _, _) =>
                      if (identifier.sameAs(variable)) {
                        val resourceVariableId: Int = ids.get(variableName) match {
                          case Some(resourceVariableId) => resourceVariableId
                          case None => ids.size
                        }
                        val newIds = ids + (variableName -> resourceVariableId)
                        val newReplacements = replacements + (command -> Use(Some(resourceVariableId), right))
                        (newReplacements, newIds)
                      }
                      else {
                        logger.error(errorMessage)
                        (replacements, ids)
                      }
                    case _ =>
                      logger.error(errorMessage)
                      (replacements, ids)
                  }
                case _ =>
                  logger.error(errorMessage)
                  (replacements, ids)
              }
            }
            else (replacements, ids)
          case _ => (replacements, ids)
        }
    })
    val newBody = replacements.foldLeft(program.mainFunction.body: BrboAst)({
      case (acc, (oldCommand, newCommand)) => BrboAstUtils.replaceAst(acc, oldCommand, newCommand)
    })
    val newMainFunction = program.mainFunction.replaceBodyWithoutGhostInitialization(newBody.asInstanceOf[Statement])
    program.replaceMainFunction(newMainFunction)
  }

  private def insertUBCheck(program: BrboProgram, boundAssertion: BoundAssertion): BrboProgram = {
    val assertion = {
      val sum: BrboExpr = program.mainFunction.approximatedResourceUsage
      val assertFunction: BrboFunction = PreDefinedFunctions.Assert.cRepresentation
      val assertion = boundAssertion.replaceResourceVariable(sum)
      FunctionCallExpr(assertFunction.identifier, List(assertion), assertFunction.returnType)
    }
    logger.infoOrError(s"Insert ub check assertion: `${assertion.printToC(0)}`")

    def generateNewUse(use: Use): List[Command] = {
      // Use the same uuid so that, we can succeed in using commands from the program without UB checks to match against
      // the refined cex. from the program with UB checks
      List(use, assertion)
    }

    replaceUses(program, generateNewUse, program.mainFunction.groupIds)
  }

  private def generateInitialAbstraction(program: BrboProgram): BrboProgram = {
    def generateNewUse(use: Use): List[Command] = {
      use.condition match {
        case Bool(b, _) => assert(b)
        case _ => throw new Exception
      }
      val newUse = Use(Some(initialAbstractionGroupId), use.update, use.condition)
      List(Reset(initialAbstractionGroupId), newUse)
    }

    replaceUses(program, generateNewUse, Set(initialAbstractionGroupId))
  }

  private def replaceUses(program: BrboProgram, f: Use => List[Command], newGroupIds: Set[Int]): BrboProgram = {
    val allCommands = BrboAstUtils.collectCommands(program.mainFunction.body)
    val replacements = allCommands.foldLeft(Map[Command, Block]())({
      (acc, command) =>
        command match {
          case use@Use(_, _, _, _) => acc + (use -> Block(f(use)))
          case _ => acc
        }
    })
    val newBody = replacements.foldLeft(program.mainFunction.body: BrboAst)({
      case (acc, (oldCode, newCode)) => BrboAstUtils.replaceAst(acc, oldCode, newCode)
    })
    val newMainFunction = {
      val f = program.mainFunction
      BrboFunction(f.identifier, f.returnType, f.parameters, newBody.asInstanceOf[Statement], newGroupIds)
    }
    program.replaceMainFunction(newMainFunction)
  }

  private def createNode(tree: SimpleDirectedGraph[TreeNode, DefaultEdge], parent: Option[TreeNode],
                         program: BrboProgram, counterexamples: Set[Path], refinement: Option[Refinement]): TreeNode = {
    assert(parent.isEmpty == refinement.isEmpty)
    val node = TreeNode(program, counterexamples, refinement, tree.vertexSet().size())
    tree.addVertex(node)
    parent match {
      case Some(value) =>
        assert(tree.containsVertex(value))
        tree.addEdge(value, node)
      case None =>
    }
    node
  }

  private def getParentNode(tree: SimpleDirectedGraph[TreeNode, DefaultEdge], node: TreeNode): Option[TreeNode] = {
    logger.infoOrError(s"Backtracking now.")
    val parents = tree.incomingEdgesOf(node).asScala
    parents.size match {
      case 0 =>
        logger.infoOrError(s"There is no parent node to backtrack to. Verification has failed!")
        None
      case 1 =>
        node.setNodeStatus(SHOULD_NOT_EXPLORE)
        Some(tree.getEdgeSource(parents.head))
      case _ => throw new Exception
    }
  }
}

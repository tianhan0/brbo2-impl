package brbo.backend.driver

import brbo.BrboMain
import brbo.backend.driver.NodeStatus._
import brbo.backend.refiner.{Refinement, Refiner}
import brbo.backend.verifier.VerifierRawResult._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.{UAutomizerVerifier, VerifierResult}
import brbo.common.ast._
import brbo.common.{CommandLineArguments, GhostVariableTyp, GhostVariableUtils, MyLogger, StringFormatUtils}
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

  def verifyFullyAmortize(upperBound: BrboExpr): VerifierResult = {
    ???
  }

  def verifyWorstCase(upperBound: BrboExpr): VerifierResult = {
    ???
  }

  def verifySelectivelyAmortize(upperBound: BrboExpr, reRefineWhenVerifierTimeout: Boolean = false): VerifierRawResult = {
    val initialAbstraction = generateInitialAbstraction(afterExtractingUses)
    val result = verify(initialAbstraction, upperBound)
    val counterexamplePath: Option[Path] = result.rawResult match {
      case TRUE_RESULT =>
        logger.infoOrError(s"Verifier successfully verifies the initial abstraction!")
        return TRUE_RESULT
      case FALSE_RESULT =>
        logger.infoOrError(s"Verifier fails to verify the initial abstraction!")
        result.counterexamplePath
      case UNKNOWN_RESULT =>
        logger.infoOrError(s"Verifier returns `$UNKNOWN_RESULT` for the initial abstraction.")
        None
    }

    // No Self-loops; No Multiple edges; No Weighted
    val tree = new SimpleDirectedGraph[TreeNode, DefaultEdge](classOf[DefaultEdge])
    val root = createNode(tree, parent = None, initialAbstraction, counterexamplePath, refinement = None)
    val refiner = new Refiner(arguments)

    @tailrec
    def helper(node: TreeNode): VerifierRawResult = {
      logger.infoOrError(s"We have explored `${tree.vertexSet().size()}` programs. We will stop at `${arguments.getMaxIterations}`.")
      if (tree.vertexSet().size() > arguments.getMaxIterations) {
        logger.infoOrError(s"Output all unknown amortizations to `${BrboMain.OUTPUT_DIRECTORY}/amortizations/`")
        tree.vertexSet().asScala.zipWithIndex.foreach({
          case (node, index) =>
            val programInC = BrboProgramInC(node.program)
            val cSourceCode = programInC.program.prettyPrintToC()
            val file = new File(s"${BrboMain.OUTPUT_DIRECTORY}/amortizations/${originalProgram.name}-${StringFormatUtils.integer(index, 3)}.txt")
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
      // Assume that all assert() in node.program are ub checks, so that we can safely remove all assert() from the cex.
      val pathWithoutUBChecks = Path.removeCommandsForUBCheck(result.counterexamplePath)
      // Refine the current node, possibly once again, based on the same counterexample
      // When a node is visited for a second (or more) time, then it must be caused by backtracking, which
      // implies that all nodes in one of its subtree have failed
      refiner.refine(node.program, pathWithoutUBChecks, upperBound, avoidRefinements) match {
        case (Some(refinedProgram), Some(refinement)) =>
          val result = verify(refinedProgram, upperBound)
          val exploreRefinedProgram: NodeStatus = result.rawResult match {
            case TRUE_RESULT | FALSE_RESULT => EXPLORING
            case UNKNOWN_RESULT => if (reRefineWhenVerifierTimeout) EXPLORING else SHOULD_NOT_EXPLORE
          }
          val newChildNode = createNode(tree, parent = Some(node), refinedProgram, pathWithoutUBChecks, Some(refinement))
          newChildNode.setNodeStatus(exploreRefinedProgram)
          if (result.rawResult == TRUE_RESULT) {
            logger.infoOrError(s"Successfully verified the sibling of the current node!")
            TRUE_RESULT
          } else {
            if (reRefineWhenVerifierTimeout) {
              logger.infoOrError(s"Explore a new child node (because verifying the current child node returns unknown) by re-refining the current node.")
              helper(node)
            }
            else {
              logger.infoOrError(s"Explore child nodes of the current child node (which has failed).")
              helper(newChildNode)
            }
          }
        case (None, None) =>
          logger.infoOrError(s"Stop refining (because no refinement can be found). Backtracking now.")
          val parents = tree.incomingEdgesOf(node).asScala
          parents.size match {
            case 0 =>
              logger.infoOrError(s"There is no parent node to backtrack to. Verification has failed!")
              FALSE_RESULT
            case 1 =>
              node.setNodeStatus(SHOULD_NOT_EXPLORE)
              val parent = tree.getEdgeSource(parents.head)
              helper(parent) // Backtrack
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    }

    helper(root)
  }

  private def verify(program: BrboProgram, upperBound: BrboExpr): VerifierResult = {
    val ubcheckInserted = insertUBCheck(program, upperBound)
    logger.infoOrError(s"Verify with upper bound `${upperBound.prettyPrintToCNoOuterBrackets}`")
    uAutomizerVerifier.verify(ubcheckInserted)
  }

  private def extractUsesFromMain(program: BrboProgram): BrboProgram = {
    val allCommands = BrboAstUtils.collectCommands(program.mainFunction.bodyNoInitialization)
    val (replacements, _) = allCommands.foldLeft((Map[Command, Command](), Map[String, Int]()))({
      case ((replacements, ids), command) =>
        command match {
          case Assignment(variable, expression, _) =>
            if (GhostVariableUtils.isGhostVariable(variable.identifier, GhostVariableTyp.Resource)) {
              val errorMessage = s"To successfully extract uses from assignments, the assignment must be in the form of " +
                s"`${variable.identifier} = ${variable.identifier} + e` for some e, instead of `${command.prettyPrintToC()}`"
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
    val newBody = replacements.foldLeft(program.mainFunction.bodyNoInitialization: BrboAst)({
      case (acc, (oldCommand, newCommand)) => BrboAstUtils.replace(acc, oldCommand, newCommand)
    })
    val newMainFunction = program.mainFunction.replaceBodyWithoutInitialization(newBody.asInstanceOf[Statement])
    program.replaceMainFunction(newMainFunction)
  }

  private def insertUBCheck(program: BrboProgram, upperBound: BrboExpr): BrboProgram = {
    logger.infoOrError(s"Insert ub check `${upperBound.prettyPrintToCNoOuterBrackets}`")
    val assertion = {
      val sum: BrboExpr = {
        def summand(id: Int): BrboExpr = {
          val (r, rSharp, rCounter) = GhostVariableUtils.generateVariables(Some(id))
          Addition(r, Multiplication(rSharp, rCounter))
        }

        program.mainFunction.groupIds.map(id => summand(id)).foldLeft(Number(0): BrboExpr)({
          (acc, summand) => Addition(acc, summand)
        })
      }
      val assertFunction: BrboFunction = PreDefinedFunctions.assert
      FunctionCall(FunctionCallExpr(assertFunction.identifier, List(LessThanOrEqualTo(sum, upperBound)), assertFunction.returnType))
    }
    logger.traceOrError(s"ub check assertion: `${assertion.prettyPrintToC()}`")

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
    val allCommands = BrboAstUtils.collectCommands(program.mainFunction.bodyNoInitialization)
    val replacements = allCommands.foldLeft(Map[Command, Block]())({
      (acc, command) =>
        command match {
          case use@Use(_, _, _, _) => acc + (use -> Block(f(use)))
          case _ => acc
        }
    })
    val newBody = replacements.foldLeft(program.mainFunction.bodyNoInitialization: BrboAst)({
      case (acc, (oldCode, newCode)) => BrboAstUtils.replace(acc, oldCode, newCode)
    })
    val newMainFunction = {
      val f = program.mainFunction
      BrboFunction(f.identifier, f.returnType, f.parameters, newBody.asInstanceOf[Statement], newGroupIds)
    }
    program.replaceMainFunction(newMainFunction)
  }

  private def createNode(tree: SimpleDirectedGraph[TreeNode, DefaultEdge], parent: Option[TreeNode],
                         program: BrboProgram, counterexample: Option[Path], refinement: Option[Refinement]): TreeNode = {
    assert(parent.isEmpty == refinement.isEmpty)
    val node = TreeNode(program, counterexample, refinement, tree.vertexSet().size())
    tree.addVertex(node)
    parent match {
      case Some(value) =>
        assert(tree.containsVertex(value))
        tree.addEdge(value, node)
      case None =>
    }
    node
  }
}

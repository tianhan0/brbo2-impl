package brbo.backend.driver

import brbo.backend.driver.NodeStatus._
import brbo.backend.refiner.{Refinement, Refiner}
import brbo.backend.verifier.VerifierRawResult._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.{UAutomizerVerifier, VerifierResult}
import brbo.common.ast._
import brbo.common.{CommandLineArguments, GhostVariableTyp, GhostVariableUtils, MyLogger}

import scala.annotation.tailrec
import scala.collection.mutable

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
    val result = verify(afterExtractingUses, upperBound, initial = true)
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

    var nodes: Set[TreeNode] = Set()

    def createNode(program: BrboProgram, counterexample: Option[Path], refinement: Option[Refinement],
                   parent: Option[TreeNode], knownChildren: mutable.Map[TreeNode, NodeStatus]): TreeNode = {
      val node = TreeNode(program, counterexample, refinement, parent, knownChildren, nodes.size)
      nodes = nodes + node
      node
    }

    val root = createNode(afterExtractingUses, counterexamplePath, None, None, mutable.Map[TreeNode, NodeStatus]())
    val refiner = new Refiner(arguments)

    @tailrec
    def helper(node: TreeNode): VerifierRawResult = {
      // Avoid siblings that should not be explored
      val avoidRefinements: Set[Refinement] = {
        node.parent match {
          case Some(parent) => parent.knownChildren.foldLeft(Set[Refinement]())({
            (acc, child) =>
              child._2 match {
                case EXPLORING => acc
                case SHOULD_NOT_EXPLORE => acc + child._1.refinement.get
              }
          })
          case None => Set()
        }
      }
      // Assume that all assert() in node.program are ub checks, so that we can safely remove all assert() from the cex.
      val pathWithoutUBChecks = Path.removeCommandsForUBCheck(result.counterexamplePath)
      // Refine the current node, possibly once again, based on the same counterexample
      // When a node is visited for a second (or more) time, then it must be caused by backtracking, which
      // implies that any of its (grand) child node has failed to verify
      refiner.refine(node.program, pathWithoutUBChecks, upperBound, avoidRefinements) match {
        case (Some(refinedProgram), Some(refinement)) =>
          val result = verify(refinedProgram, upperBound, initial = false)
          val exploreRefinedProgram: NodeStatus = result.rawResult match {
            case TRUE_RESULT | FALSE_RESULT => EXPLORING
            case UNKNOWN_RESULT => if (reRefineWhenVerifierTimeout) EXPLORING else SHOULD_NOT_EXPLORE
          }
          val newSiblingNode = createNode(refinedProgram, pathWithoutUBChecks,
            Some(refinement), node.parent, mutable.Map[TreeNode, NodeStatus]())
          node.parent match {
            case Some(parent) =>
              parent.knownChildren.+=(newSiblingNode -> exploreRefinedProgram)
              assert(parent.knownChildren.contains(newSiblingNode)) // TODO: Remove this!
            case None => // The current node is the root (which contains the initial abstraction)
          }
          if (result.rawResult == TRUE_RESULT) {
            logger.infoOrError(s"Successfully verified the sibling of the current node!")
            TRUE_RESULT
          } else {
            if (reRefineWhenVerifierTimeout) {
              logger.infoOrError(s"Re-refine the current node, because we decide to re-refine when the verifier times out.")
              helper(node) // Re-refine the current node
            }
            else {
              logger.infoOrError(s"Explore the sibling node.")
              helper(newSiblingNode) // Explore the sibling node
            }
          }
        case (None, None) =>
          logger.infoOrError(s"Stop refining (because no refinement can be found). Backtracking now.")
          node.parent match {
            case Some(parent) =>
              parent.knownChildren.+=(node -> SHOULD_NOT_EXPLORE)
              helper(parent) // Backtrack
            case None =>
              logger.infoOrError(s"There is no parent node to backtrack to. Verification has failed!")
              FALSE_RESULT
          }
        case _ => throw new Exception
      }
    }

    helper(root)
  }

  private def verify(program: BrboProgram, boundAssertion: BrboExpr, initial: Boolean): VerifierResult = {
    val ubcheckInserted = insertUBCheck(program, boundAssertion, initial)
    logger.infoOrError(s"Verify with upper bound `${boundAssertion.prettyPrintToCNoOuterBrackets}`")
    uAutomizerVerifier.verify(ubcheckInserted)
  }

  private def extractUsesFromMain(program: BrboProgram): BrboProgram = {
    val allCommands = BrboAstUtils.collectCommands(program.mainFunction.bodyWithoutInitialization)
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
    val newBody = replacements.foldLeft(program.mainFunction.bodyWithoutInitialization: BrboAst)({
      case (acc, (oldCommand, newCommand)) => BrboAstUtils.replace(acc, oldCommand, newCommand)
    })
    val newMainFunction = program.mainFunction.replaceBodyWithoutInitialization(newBody.asInstanceOf[Statement])
    program.replaceMainFunction(newMainFunction)
  }

  private def insertUBCheck(program: BrboProgram, upperBound: BrboExpr, initialize: Boolean): BrboProgram = {
    logger.infoOrError(s"Insert ub check `${upperBound.prettyPrintToCNoOuterBrackets}`. Initialize? `$initialize`")
    val assertion = {
      val sum: BrboExpr = {
        def summand(id: Int): BrboExpr = {
          val (r, rSharp, rCounter) = GhostVariableUtils.generateVariables(Some(id))
          Addition(r, Multiplication(rSharp, rCounter))
        }
        val groupIds = if (initialize) Set(initialAbstractionGroupId) else program.mainFunction.groupIds
        groupIds.map(id => summand(id)).foldLeft(Number(0): BrboExpr)({
          (acc, summand) => Addition(acc, summand)
        })
      }
      val assertFunction: BrboFunction = PreDefinedFunctions.assert
      FunctionCall(FunctionCallExpr(assertFunction.identifier, List(LessThanOrEqualTo(sum, upperBound)), assertFunction.returnType))
    }
    logger.traceOrError(s"ub check assertion: `${assertion.prettyPrintToC()}`")
    val allCommands = BrboAstUtils.collectCommands(program.mainFunction.bodyWithoutInitialization)
    val replacements = allCommands.foldLeft(Map[Command, Block]())({
      (acc, command) =>
        command match {
          case Use(groupId, update, condition, _) =>
            condition match {
              case Bool(b, _) => if (initialize) assert(b)
              case _ => throw new Exception
            }
            val extraReset = if (initialize) List(Reset(initialAbstractionGroupId)) else Nil
            val newUse = Use(if (initialize) Some(initialAbstractionGroupId) else groupId, update, condition)
            val block = Block(extraReset ::: List(newUse, assertion))
            acc + (command -> block)
          case Reset(_, _, _) => throw new Exception
          case _ => acc
        }
    })
    val newBody = replacements.foldLeft(program.mainFunction.bodyWithoutInitialization: BrboAst)({
      case (acc, (oldCode, newCode)) => BrboAstUtils.replace(acc, oldCode, newCode)
    })
    val newMainFunction = {
      val f = program.mainFunction
      val groupIds = if (initialize) Set(initialAbstractionGroupId) else f.groupIds
      BrboFunction(f.identifier, f.returnType, f.parameters, newBody.asInstanceOf[Statement], groupIds)
    }
    program.replaceMainFunction(newMainFunction)
  }
}

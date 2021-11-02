package brbo.backend.driver

import brbo.backend.driver.NodeStatus._
import brbo.backend.refiner.{Refinement, Refiner}
import brbo.backend.verifier.VerifierRawResult._
import brbo.backend.verifier.cex.Path
import brbo.backend.verifier.{UAutomizerVerifier, VerifierResult}
import brbo.common.ast.{BrboExpr, BrboProgram}
import brbo.common.{CommandLineArguments, MyLogger}

import scala.annotation.tailrec
import scala.collection.mutable

class Driver(arguments: CommandLineArguments, originalProgram: BrboProgram) {
  private val logger = MyLogger.createLogger(classOf[Driver], arguments.getDebugMode)
  private val uAutomizerVerifier = new UAutomizerVerifier(arguments)
  private val initialAbstraction: BrboProgram = {
    ???
  }

  def verifyFullyAmortize(boundAssertion: BrboExpr): VerifierResult = {
    ???
  }

  def verifyWorstCase(boundAssertion: BrboExpr): VerifierResult = {
    ???
  }

  def verifySelectivelyAmortize(boundAssertion: BrboExpr, reRefineWhenVerifierTimeout: Boolean = false): VerifierRawResult = {
    val refiner = new Refiner(originalProgram, arguments)
    val result = verify(initialAbstraction, boundAssertion)
    val counterexamplePath: Option[Path] = result.rawResult match {
      case TRUE_RESULT =>
        logger.info(s"Verifier successfully verifies the initial abstraction!")
        return TRUE_RESULT
      case FALSE_RESULT =>
        logger.info(s"Verifier fails to verify the initial abstraction!")
        result.counterexamplePath
      case UNKNOWN_RESULT =>
        logger.info(s"Verifier returns `$UNKNOWN_RESULT` for the initial abstraction.")
        None
    }

    var nodes: Set[TreeNode] = Set()

    def createNode(program: BrboProgram, counterexample: Option[Path], refinement: Option[Refinement],
                   parent: Option[TreeNode], knownChildren: mutable.Map[TreeNode, NodeStatus]): TreeNode = {
      val node = TreeNode(program, counterexample, refinement, parent, knownChildren, nodes.size)
      nodes = nodes + node
      node
    }

    val root = createNode(initialAbstraction, counterexamplePath, None, None, mutable.Map[TreeNode, NodeStatus]())

    @tailrec
    def helper(node: TreeNode): VerifierRawResult = {
      val avoidRefinements: Set[Refinement] = node.knownChildren.foldLeft(Set[Refinement]())({
        (acc, child) =>
          child._2 match {
            case EXPLORING => acc
            case SHOULD_NOT_EXPLORE => acc + child._1.refinement.get
          }
      })
      // Refine the current node, possibly once again, based on the same counterexample
      // When a node is visited for a second (or more) time, then it must be caused by backtracking, which
      // implies that any of its (grand) child node has failed to verify
      refiner.refine(node.program, node.counterexample, boundAssertion, avoidRefinements) match {
        case (Some(refinedProgram), Some(refinement)) =>
          val result = verify(refinedProgram, boundAssertion)
          val exploreRefinedProgram: NodeStatus = result.rawResult match {
            case TRUE_RESULT | FALSE_RESULT => EXPLORING
            case UNKNOWN_RESULT => if (reRefineWhenVerifierTimeout) EXPLORING else SHOULD_NOT_EXPLORE
          }
          val newSiblingNode = createNode(refinedProgram, result.counterexamplePath,
            Some(refinement), node.parent, mutable.Map[TreeNode, NodeStatus]())
          node.parent match {
            case Some(parent) =>
              parent.knownChildren.+=(newSiblingNode -> exploreRefinedProgram)
              assert(parent.knownChildren.contains(newSiblingNode)) // TODO: Remove this!
            case None => // The current node is the root (which contains the initial abstraction)
          }
          if (result.rawResult == TRUE_RESULT) {
            logger.info(s"Successfully verified the sibling of the current node!")
            TRUE_RESULT
          } else {
            if (reRefineWhenVerifierTimeout) {
              logger.info(s"Re-refine the current node, because we decide to re-refine when the verifier times out.")
              helper(node) // Re-refine the current node
            }
            else {
              logger.info(s"Explore the sibling node.")
              helper(newSiblingNode) // Explore the sibling node
            }
          }
        case (None, None) =>
          logger.info(s"Stop refining (because no refinement can be found). Backtracking now.")
          node.parent match {
            case Some(parent) =>
              parent.knownChildren.+=(node -> SHOULD_NOT_EXPLORE)
              helper(parent) // Backtrack
            case None =>
              logger.info(s"There is no parent node to backtrack to. Verification has failed!")
              FALSE_RESULT
          }
        case _ => throw new Exception
      }
    }

    helper(root)
  }

  private def verify(program: BrboProgram, boundAssertion: BrboExpr): VerifierResult = {
    val ubCheckInserted = insertUBCheck(program, boundAssertion)
    uAutomizerVerifier.verify(ubCheckInserted)
  }

  private def insertUBCheck(program: BrboProgram, boundAssertion: BrboExpr): BrboProgram = {
    ???
  }
}

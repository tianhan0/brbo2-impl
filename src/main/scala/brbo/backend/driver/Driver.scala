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

class Driver(commandLineArguments: CommandLineArguments, originalProgram: BrboProgram) {
  private val logger = MyLogger.createLogger(classOf[Driver], commandLineArguments.getDebugMode)
  private val uAutomizerVerifier = new UAutomizerVerifier(commandLineArguments)

  def verifyFullyAmortize(boundAssertion: BrboExpr): VerifierResult = {
    ???
  }

  def verifyWorstCase(boundAssertion: BrboExpr): VerifierResult = {
    ???
  }

  def verifySelectivelyAmortize(boundAssertion: BrboExpr): VerifierRawResult = {
    val refiner = new Refiner(originalProgram, commandLineArguments)

    val initialAbstraction: BrboProgram = ???
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
    def helper(node: TreeNode, refineWhenUnknown: Boolean): VerifierRawResult = {
      val avoidRefinements: Set[Refinement] = node.knownChildren.foldLeft(Set[Refinement]())({
        (acc, child) =>
          child._2 match {
            case EXPLORING => acc
            case SHOULD_NOT_EXPLORE => acc + child._1.refinement.get
          }
      })
      // Refine the current node (possibly once again)
      refiner.refine(node.program, node.counterexample, boundAssertion, avoidRefinements) match {
        case (Some(refinedProgram), Some(refinement)) =>
          val result = verify(refinedProgram, boundAssertion)
          val (counterexamplePath: Option[Path], exploreRefinedProgram: NodeStatus) = result.rawResult match {
            case TRUE_RESULT => return TRUE_RESULT
            case FALSE_RESULT => (result.counterexamplePath, EXPLORING)
            case UNKNOWN_RESULT => (result.counterexamplePath, if (refineWhenUnknown) EXPLORING else SHOULD_NOT_EXPLORE)
          }
          val newChildNode = createNode(refinedProgram, counterexamplePath, Some(refinement), Some(node), mutable.Map[TreeNode, NodeStatus]())
          root.knownChildren.+=(newChildNode -> exploreRefinedProgram)
          assert(root.knownChildren.contains(newChildNode)) // TODO: Remove this!

          if (!refineWhenUnknown) {
            logger.info(s"Re-refine the current node, since refineWhenUnknown is `$refineWhenUnknown`.")
            helper(node, refineWhenUnknown) // Re-refine the current node
          }
          else {
            logger.info(s"Explore the child node.")
            helper(newChildNode, refineWhenUnknown) // Explore the child node
          }
        case (None, None) =>
          logger.info(s"Stop refining. Backtracking now.")
          node.parent match {
            case Some(parent) =>
              parent.knownChildren.+=(node -> SHOULD_NOT_EXPLORE)
              helper(parent, refineWhenUnknown) // Backtrack
            case None =>
              logger.info(s"There is no more parent node to backtrack to. Verification has failed!")
              FALSE_RESULT
          }
        case _ => throw new Exception
      }
    }

    helper(root, refineWhenUnknown = true)
  }

  private def verify(program: BrboProgram, boundAssertion: BrboExpr): VerifierResult = {
    val ubCheckInserted = insertUBCheck(program, boundAssertion)
    uAutomizerVerifier.verify(ubCheckInserted)
  }

  private def insertUBCheck(program: BrboProgram, boundAssertion: BrboExpr): BrboProgram = {
    ???
  }
}

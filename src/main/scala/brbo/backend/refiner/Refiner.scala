package brbo.backend.refiner

import brbo.backend.verifier.AbstractInterpreter
import brbo.backend.verifier.VerifierStatus.VerifierStatus
import brbo.backend.verifier.cex.Path
import brbo.common._
import brbo.common.ast.{BoundAssertion, BrboProgram}

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class Refiner(arguments: CommandLineArguments) {
  protected val logger: MyLogger = MyLogger.createLogger(classOf[Refiner], arguments.getDebugMode)
  protected val pathRefinement = new PathRefinement(arguments)

  def refine(originalProgram: BrboProgram, path: Path,
             boundAssertion: BoundAssertion, refinementsToAvoid: Set[Refinement]): (Option[BrboProgram], Option[Refinement]) = {
    val inputVariables = originalProgram.mainFunction.parameters
    val allRefinements = pathRefinement.refine(path, originalProgram.mainFunction.identifier)
    logger.infoOrError(s"Generated `${allRefinements.size}` path refinements")
    refinementsToAvoid.foreach(r => logger.traceOrError(s"Avoid refinement: ${r.toStringNoPath}"))

    def validateRefinement(refinement: Refinement): Option[(Refinement, VerifierStatus)] = {
      logger.traceOrError(s"Generated refinement: ${refinement.toStringNoPath}")
      // It is expected that, the refined path is empty when there is no refinement (over the original path)
      if (refinement.noRefinement || refinementsToAvoid.exists(r => r.sameAs(refinement))) {
        None
      } else {
        val refinedPath = refinement.refinedPath(originalProgram.mainFunction)
        // logger.traceOrError(s"Validate refined path:\n`${refinedPath.mkString("\n")}`")
        // Using the model checker for verification
        val assertion = {
          val allGroupIds: Set[Int] = // Get all group IDs and then all ghost variables
            originalProgram.mainFunction.groupIds ++ refinement.groupIds.values.flatten
          val sum = GhostVariableUtils.approximatedResourceUsage(allGroupIds)
          logger.traceOrError(s"Approximated resource usage: `${sum.prettyPrintToC()}`")
          boundAssertion.replaceResourceVariable(sum)
        }
        val result = AbstractInterpreter.verifyPath(refinedPath, assertion, inputVariables, arguments)
        Some((refinement, result.result.rawResult))
      }
    }

    logger.infoOrError(s"Search for a refinement for path `$path`.")
    /*implicit val executionContext: ExecutionContext = {
      if (arguments.getThreads == CommandLineArguments.DEFAULT_NUMBER_OF_THREADS) scala.concurrent.ExecutionContext.Implicits.global
      else ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(arguments.getThreads))
    }
    val futures = Future.traverse(allRefinements)({
      refinement => Future {
        validateRefinement(refinement)
      }
    })*/

    val results = allRefinements.map(r => validateRefinement(r))
    // val results = Await.result(futures, Duration.Inf)
    val programSynthesis = new Synthesizer(originalProgram, arguments)
    val numberOfSuccessfulPathRefinements = results.count({
      case Some(value) => value._2 == brbo.backend.verifier.VerifierStatus.TRUE_RESULT
      case None => false
    })
    logger.infoOrError(s"Number of successful path refinements: `$numberOfSuccessfulPathRefinements`")
    results.foreach({
      case Some((refinement: Refinement, result: VerifierStatus)) =>
        // TODO: Try to release memory
        // result.releaseMemory()
        result match {
          case brbo.backend.verifier.VerifierStatus.TRUE_RESULT =>
            try {
              val newProgram = programSynthesis.synthesize(refinement)
              return (Some(newProgram), Some(refinement))
            }
            catch {
              case _: SynthesisFailException =>
                logger.infoOrError(s"Cannot synthesize a program from the current path refinement. Will try a new path refinement.")
            }
          case _ =>
        }
      case None =>
    })
    if (numberOfSuccessfulPathRefinements == 0) logger.infoOrError(s"No successful path refinements")
    else logger.infoOrError(s"No successful synthesis (from path refinements)")
    (None, None)
  }
}

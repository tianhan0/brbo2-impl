package brbo.backend.refiner

import brbo.backend.verifier.SymbolicExecution
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.BOOL
import brbo.common.GhostVariableTyp._
import brbo.common.ast.{BrboExpr, BrboProgram}
import brbo.common.{CommandLineArguments, GhostVariableUtils, MyLogger}
import com.microsoft.z3.{AST, Expr}

class Refiner(originalProgram: BrboProgram, commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[Refiner], commandLineArguments.getDebugMode)
  private val pathRefinement = new PathRefinement(commandLineArguments, originalProgram.mainFunction)
  private val programSynthesis = new ProgramSynthesis(commandLineArguments)

  def refine(refinedProgram: BrboProgram, counterexamplePath: Option[Path],
             boundAssertion: BrboExpr, avoidRefinements: Set[Refinement]): (Option[BrboProgram], Option[Refinement]) = {
    logger.info(s"Refine ${if (counterexamplePath.isEmpty) "without" else "with"} a counterexample path")
    counterexamplePath match {
      case Some(counterexamplePath2) =>
        val symbolicExecution = new SymbolicExecution(refinedProgram.mainFunction.parameters)
        val solver = symbolicExecution.solver
        val boundAssertionAST = boundAssertion.toZ3AST(solver)
        val refinementsMap = pathRefinement.refine(counterexamplePath2).foldLeft(Map[Expr, (Refinement, Expr)]())({
          (acc, refinement) =>
            if (avoidRefinements.contains(refinement)) acc
            else {
              val finalState = symbolicExecution.execute(refinement.getRefinedPath)
              // Get all group IDs and then all ghost variables
              val allGroupIds: Set[Int] = refinedProgram.groupIds -- refinement.groupIDs.keys ++ refinement.groupIDs.values.flatten
              var sum: AST = solver.mkIntVal(0)
              allGroupIds.foreach({
                groupId =>
                  val counter: AST = {
                    val v = GhostVariableUtils.generateVariable(Some(groupId), Counter)
                    finalState.valuations.head.getOrElse(v.identifier, throw new Exception)._2.v
                  }
                  val sharp: AST = {
                    val v = GhostVariableUtils.generateVariable(Some(groupId), Sharp)
                    finalState.valuations.head.getOrElse(v.identifier, throw new Exception)._2.v
                  }
                  val resource: AST = {
                    val v = GhostVariableUtils.generateVariable(Some(groupId), Resource)
                    finalState.valuations.head.getOrElse(v.identifier, throw new Exception)._2.v
                  }
                  sum = solver.mkAdd(sum, resource, solver.mkMul(counter, sharp))
              })
              val assertion = solver.mkLe(sum, boundAssertionAST)
              acc + (symbolicExecution.createFreshVariable(BOOL)._2 -> (refinement, assertion))
            }
        })

        logger.info(s"Search for a path refinement for path `$counterexamplePath2`.")
        // Keep finding new path transformations until either finding a program transformation that can realize it,
        // or there exists no program transformation that can realize any path transformation
        var avoidRefinement2: Set[Refinement] = Set()
        while (avoidRefinement2.size < refinementsMap.size) {
          var nameDisjunctions = solver.mkTrue()
          var disjunction = solver.mkTrue()
          refinementsMap.foreach({
            case (variableAST: AST, (refinement: Refinement, assertion: Expr)) =>
              // Add a new disjunct
              if (!avoidRefinement2.contains(refinement)) {
                disjunction = solver.mkOr(disjunction, assertion)
                nameDisjunctions = solver.mkAnd(nameDisjunctions, solver.mkIff(variableAST, assertion))
              }
          })
          val z3Result = solver.checkAssertionPushPop(solver.mkAnd(disjunction, nameDisjunctions), printUnsatCore = false)
          if (z3Result) {
            val model = solver.getModel
            val goodRefinements = refinementsMap.filter({ case (variableAST: AST, _) => model.eval(variableAST, false).toString == "true" })
            val refinement = goodRefinements.head._2._1
            try {
              val newProgram = programSynthesis.synthesize(refinedProgram, refinement, symbolicExecution)
              return (Some(newProgram), Some(refinement))
            }
            catch {
              case _: SynthesisFailException =>
                logger.info(s"Cannot synthesize a program from the current path refinement. Will try a new path refinement.")
                avoidRefinement2 = avoidRefinement2 + refinement
            }
          } else {
            logger.info(s"Cannot find a path refinement, if avoiding the ones cannot be synthesized into programs.")
            return (None, None)
          }
        }
        logger.info(s"Tried all path refinements, but none of them can be synthesized into a program.")
        (None, None)
      case None => ??? // Simply try a completely new program?
    }
  }
}

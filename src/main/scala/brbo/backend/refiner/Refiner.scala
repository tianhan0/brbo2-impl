package brbo.backend.refiner

import brbo.backend.verifier.SymbolicExecution
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.BOOL
import brbo.common.GhostVariableTyp._
import brbo.common.ast.{BrboExpr, BrboProgram}
import brbo.common.{CommandLineArguments, GhostVariableUtils, MyLogger}
import com.microsoft.z3.{AST, Expr}

class Refiner(originalProgram: BrboProgram, arguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[Refiner], arguments.getDebugMode)
  private val pathRefinement = new PathRefinement(arguments, originalProgram.mainFunction)

  def refine(refinedProgram: BrboProgram, counterexamplePath: Option[Path],
             boundAssertion: BrboExpr, avoidRefinementsInCegar: Set[Refinement]): (Option[BrboProgram], Option[Refinement]) = {
    logger.infoOrError(s"Refine ${if (counterexamplePath.isEmpty) "without" else "with"} a counterexample path")
    counterexamplePath match {
      case Some(counterexamplePath2) =>
        val symbolicExecution = new SymbolicExecution(refinedProgram.mainFunction.parameters)
        val solver = symbolicExecution.solver
        logger.infoOrError(s"Generating all possible path refinements")
        val refinementsMap = pathRefinement.refine(counterexamplePath2).foldLeft(Map[Expr, (Refinement, Expr)]())({
          (acc, refinement) =>
            if (refinement.noRefinement || avoidRefinementsInCegar.contains(refinement)) acc
            else {
              val refinedPath = refinement.refinedPath(refinedProgram.mainFunction)
              logger.infoOrError(s"Symbolically executing refined path:\n`${refinedPath.mkString("\n")}`")
              val finalState = symbolicExecution.execute(refinedPath)
              logger.traceOrError(s"Final state:\n`$finalState`")
              // Get all group IDs and then all ghost variables
              val allGroupIds: List[Int] = (refinedProgram.mainFunction.groupIds -- refinement.groupIds.keys ++ refinement.groupIds.values.flatten).toList.sorted
              logger.traceOrError(s"All groups considered: `$allGroupIds`")
              var sum: AST = solver.mkIntVal(0)
              allGroupIds.foreach({
                groupId =>
                  val counter: AST = solver.mkIntVar(GhostVariableUtils.generateVariable(Some(groupId), Counter).identifier)
                  val sharp: AST = solver.mkIntVar(GhostVariableUtils.generateVariable(Some(groupId), Sharp).identifier)
                  val resource: AST = solver.mkIntVar(GhostVariableUtils.generateVariable(Some(groupId), Resource).identifier)
                  sum = solver.mkAdd(sum, resource, solver.mkMul(counter, sharp))
              })
              val assertion = solver.mkImplies(
                solver.mkAnd(SymbolicExecution.valuationToAST(finalState.valuations.head, solver), finalState.pathCondition),
                solver.mkLe(sum, boundAssertion.toZ3AST(solver))
              )
              acc + (symbolicExecution.createFreshVariable(BOOL)._2 -> (refinement, assertion))
            }
        })

        logger.infoOrError(s"Search for a successful refinement for path `$counterexamplePath2`.")
        val programSynthesis = new ProgramSynthesis(refinedProgram, arguments.getRelationalPredicates, arguments)
        // Keep finding new path transformations until either finding a program transformation that can realize it,
        // or there exists no program transformation that can realize any path transformation
        var avoidRefinementInSynthesis: Set[Refinement] = Set()
        while (avoidRefinementInSynthesis.size < refinementsMap.size) {
          val query = {
            var disjunction = solver.mkTrue()
            var iffConjunction = solver.mkTrue()
            refinementsMap.foreach({
              case (variableAST: AST, (refinement: Refinement, assertion: Expr)) =>
                // Add a new disjunct
                if (!avoidRefinementInSynthesis.contains(refinement)) {
                  disjunction = solver.mkOr(disjunction, assertion)
                  iffConjunction = solver.mkAnd(iffConjunction, solver.mkIff(variableAST, assertion))
                }
            })
            val inputs = symbolicExecution.inputs.values.map(pair => pair._2.v)
            solver.mkForall(inputs, solver.mkAnd(disjunction, iffConjunction))
          }
          val z3Result = solver.checkAssertionPushPop(query, arguments.getDebugMode)
          if (z3Result) {
            val model = solver.getModel
            val goodRefinements = refinementsMap.filter({ case (variableAST: AST, _) => model.eval(variableAST, false).toString == "true" })
            val refinement = goodRefinements.head._2._1
            try {
              val newProgram = programSynthesis.synthesize(refinement)
              return (Some(newProgram), Some(refinement))
            }
            catch {
              case _: SynthesisFailException =>
                logger.infoOrError(s"Cannot synthesize a program from the current path refinement. Will try a new path refinement.")
                avoidRefinementInSynthesis = avoidRefinementInSynthesis + refinement
            }
          } else {
            logger.infoOrError(s"Cannot find a path refinement, if avoiding the ones cannot be synthesized into programs.")
            return (None, None)
          }
        }
        logger.infoOrError(s"Tried all path refinements, but none of them can be synthesized into a program.")
        (None, None)
      case None =>
        // Or randomly try a completely new program?
        throw new Exception("Don't know how to refine if there is no counterexample!")
    }
  }
}

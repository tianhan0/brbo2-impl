package brbo.backend.refiner

import brbo.backend.verifier.SymbolicExecution
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.BOOL
import brbo.common.GhostVariableTyp._
import brbo.common.ast.{BrboExpr, BrboProgram}
import brbo.common.{CommandLineArguments, GhostVariableUtils, MyLogger}
import com.microsoft.z3.{AST, Expr}

class Refiner(arguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[Refiner], arguments.getDebugMode)
  private val pathRefinement = new PathRefinement(arguments)

  def refine(programToRefine: BrboProgram, counterexamplePath: Path, boundAssertion: BrboExpr,
             avoidRefinementsInCegar: Set[Refinement]): (Option[BrboProgram], Option[Refinement]) = {
    val symbolicExecution = new SymbolicExecution(programToRefine.mainFunction.parameters)
    val solver = symbolicExecution.solver
    logger.infoOrError(s"Generating all possible path refinements")
    val refinementsMap = pathRefinement.refine(counterexamplePath, programToRefine.mainFunction.identifier).foldLeft(Map[Expr, (Refinement, Expr)]())({
      (acc, refinement) =>
        // It is expected that, the refined path is empty when there is no refinement (over the original path)
        if (refinement.noRefinement || avoidRefinementsInCegar.contains(refinement)) acc
        else {
          val refinedPath = refinement.refinedPath(programToRefine.mainFunction)
          logger.traceOrError(s"Symbolically executing refined path:\n`${refinedPath.mkString("\n")}`")
          val finalState = symbolicExecution.execute(refinedPath)
          logger.traceOrError(s"Final state:\n`$finalState`")
          // Get all group IDs and then all ghost variables
          val allGroupIds: List[Int] = (programToRefine.mainFunction.groupIds -- refinement.groupIds.keys ++ refinement.groupIds.values.flatten).toList.sorted
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

    logger.infoOrError(s"Search for a successful refinement for path `$counterexamplePath`.")
    val programSynthesis = new Synthesizer(programToRefine, arguments.getRelationalPredicates, arguments)
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
  }
}

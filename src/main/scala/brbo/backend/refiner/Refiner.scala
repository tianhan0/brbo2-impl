package brbo.backend.refiner

import brbo.backend.verifier.AbstractInterpreter
import brbo.backend.verifier.InterpreterKind.InterpreterKind
import brbo.backend.verifier.cex.Path
import brbo.common.GhostVariableTyp.{Counter, Resource, Star}
import brbo.common.ast.{BoundAssertion, BrboProgram}
import brbo.common._
import com.microsoft.z3.{AST, Expr}

class Refiner(arguments: CommandLineArguments) {
  protected val logger: MyLogger = MyLogger.createLogger(classOf[Refiner], arguments.getDebugMode)
  protected val pathRefinement = new PathRefinement(arguments)

  def refine(originalProgram: BrboProgram, path: Path,
             boundAssertion: BoundAssertion, refinementsToAvoid: Set[Refinement],
             interpreterKind: InterpreterKind): (Option[BrboProgram], Option[Refinement]) = {
    val inputVariables = originalProgram.mainFunction.parameters
    val solver = new Z3Solver
    logger.infoOrError(s"Generate all possible path refinements")
    var declaredVariables = Set[String]()
    var refinements = List[(Refinement, Expr)]()
    pathRefinement.refine(path, originalProgram.mainFunction.identifier).foreach({
      refinement =>
        // It is expected that, the refined path is empty when there is no refinement (over the original path)
        if (refinement.noRefinement || refinementsToAvoid.contains(refinement)) {
          ;
        } else {
          val refinedPath = refinement.refinedPath(originalProgram.mainFunction)
          logger.traceOrError(s"Validate refined path:\n`${refinedPath.mkString("\n")}`")
          // Get all group IDs and then all ghost variables
          val (finalState, newVariables) = AbstractInterpreter.interpretPath(refinedPath, inputVariables, solver, interpreterKind, arguments)
          declaredVariables = declaredVariables ++ newVariables
          val allGroupIds: List[Int] =
            (originalProgram.mainFunction.groupIds ++ refinement.groupIds.values.flatten).toList.sorted
          logger.traceOrError(s"All groups considered: `$allGroupIds`")
          var sum: AST = solver.mkIntVal(0)
          allGroupIds.foreach({
            groupId =>
              val counter: AST = solver.mkIntVar(GhostVariableUtils.generateVariable(Some(groupId), Counter).name)
              val star: AST = solver.mkIntVar(GhostVariableUtils.generateVariable(Some(groupId), Star).name)
              val resource: AST = solver.mkIntVar(GhostVariableUtils.generateVariable(Some(groupId), Resource).name)
              sum = solver.mkAdd(sum, resource, solver.mkMul(counter, star))
          })
          val assertion = solver.mkImplies(
            finalState,
            solver.substitute(boundAssertion.assertion.toZ3AST(solver), solver.mkIntVar(boundAssertion.resourceVariable), sum)
          )
          refinements = (refinement, assertion) :: refinements
        }
    })
    val refinementsMap = refinements.foldLeft(Map[Expr, (Refinement, Expr)]())({
      (acc, pair) =>
        val name = s"v!!${declaredVariables.size}" // Variable names seem to affect which path is selected by Z3
        assert(!declaredVariables.contains(name))
        declaredVariables = declaredVariables + name
        val boolVar = solver.mkBoolVar(name)
        acc + (boolVar -> pair)
    })

    logger.infoOrError(s"Search for a refinement for path `$path`.")
    val programSynthesis = new Synthesizer(originalProgram, arguments)
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
        val inputs = AbstractInterpreter.getInputVariables(inputVariables, solver)
        solver.mkForall(inputs, solver.mkAnd(disjunction, iffConjunction))
      }
      val z3Result = solver.checkAssertionPushPop(query, arguments.getDebugMode)
      if (z3Result) {
        val model = solver.getModel
        val goodRefinements = refinementsMap.filter({ case (variableAST: AST, _) => model.eval(variableAST, false).toString == "true" })
        if (goodRefinements.isEmpty) {
          logger.infoOrError(s"Cannot find the path refinement, even though Z3 returns `$z3Result`. Model: `$model`.")
          return (None, None)
        }
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

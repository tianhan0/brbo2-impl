package brbo.backend.refiner

import brbo.backend.verifier.SymbolicExecution
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.INT
import brbo.common.ast.{BrboExpr, BrboProgram}
import brbo.common.{CommandLineArguments, MyLogger}
import com.microsoft.z3.AST

class Refiner(originalProgram: BrboProgram, commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[Refiner], commandLineArguments.getDebugMode)
  private val pathRefinement = new PathRefinement(commandLineArguments, originalProgram.mainFunction)
  private val programSynthesis = new ProgramSynthesis(commandLineArguments)

  def refine(refinedProgram: BrboProgram, counterexamplePath: Option[Path], boundAssertion: BrboExpr, avoidRefinements: Set[Refinement]): (Option[BrboProgram], Option[Refinement]) = {
    counterexamplePath match {
      case Some(counterexamplePath2) =>
        val symbolicExecution = new SymbolicExecution(refinedProgram.mainFunction.parameters)
        val solver = symbolicExecution.solver
        val refinements = (pathRefinement.refine(counterexamplePath2) -- avoidRefinements).foldLeft(Map[Refinement, (String, AST)]())({
          (acc, refinement) => acc + (refinement -> symbolicExecution.createFreshVariable(INT))
        })
        var disjunction = solver.mkTrue()
        refinements.map({
          case (refinement: Refinement, (variableName: String, z3AST: AST)) =>
            val finalState = symbolicExecution.execute(refinement.getRefinedPath)
            // Get all group IDs and then all ghost variables
            finalState.valuations.head.get(???)
            // Add a new disjunct
            ???
        })

        val z3Result = solver.checkAssertionPushPop(disjunction, printUnsatCore = false)
        if (z3Result) {
          val model = solver.getModel
          model.eval(???, false)
          // Keep finding new path transformations until we find a program transformation that can realize it
          var newProgram: Option[BrboProgram] = programSynthesis.synthesize(refinedProgram, ???)
          newProgram
        } else {
          logger.fatal(s"There exist no more path refinement!")
          None
        }
      case None => ??? // Simply try a completely new program?
    }
  }
}

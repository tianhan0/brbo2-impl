package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.{CommandLineArguments, MyLogger}
import brbo.common.ast.{BrboExpr, BrboProgram}

class Refiner(brboProgram: BrboProgram, commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[Refiner], commandLineArguments.getDebugMode)
  private val pathTransformation = new PathRefinementOld(commandLineArguments, brboProgram)
  private val programTransformation = new ProgramSynthesis(commandLineArguments)

  def refine(brboProgram2: BrboProgram, counterexamplePath: Option[Path]): BrboProgram = {
    counterexamplePath match {
      case Some(counterexamplePath2) =>
        // Keep finding new path transformations until we find a program transformation that can realize it
        var newProgram: Option[BrboProgram] = None
        while (newProgram.isEmpty) {
          val paths = pathTransformation.refine(counterexamplePath2)
          newProgram = programTransformation.synthesize(brboProgram2, paths)
        }
        newProgram.get
      case None => ??? // Simply try a completely new program?
    }
  }
}

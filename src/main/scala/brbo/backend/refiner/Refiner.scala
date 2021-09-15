package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.{CommandLineArguments, MyLogger}
import brbo.common.ast.{BrboExpr, BrboProgram}

class Refiner(boundAssertion: BrboExpr, commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[Refiner], commandLineArguments.getDebugMode)
  private val pathTransformation = new PathTransformation(commandLineArguments)
  private val programTransformation = new ProgramTransformation(commandLineArguments)

  def refine(brboProgram: BrboProgram, counterexamplePath: Option[Path]): BrboProgram = {
    counterexamplePath match {
      case Some(counterexamplePath2) =>
        // Keep finding new path transformations until we find a program transformation that can realize it
        var newProgram: Option[BrboProgram] = None
        while (newProgram.isEmpty) {
          val paths = pathTransformation.pathTransformation(counterexamplePath2)
          newProgram = programTransformation.programTransformation(brboProgram, paths)
        }
        newProgram.get
      case None => ??? // Simply try a completely new program?
    }
  }
}

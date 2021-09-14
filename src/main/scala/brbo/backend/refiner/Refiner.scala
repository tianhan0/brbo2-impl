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
        val paths = pathTransformation.pathTransformation(counterexamplePath2)
        val newProgram = programTransformation.programTransformation(brboProgram, paths)
        newProgram
      case None => ??? // Simply try a completely new program?
    }
  }
}

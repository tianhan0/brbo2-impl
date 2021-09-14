package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.{CommandLineArguments, MyLogger}
import brbo.common.ast.Command

class PathTransformation(commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[PathTransformation], commandLineArguments.getDebugMode)

  // Perform command transformations to commands in the given path
  def pathTransformation(path: Path): Set[Path] = {
    ???
  }

  def commandTransformation(command: Command): Set[Command] = {
    ???
  }
}

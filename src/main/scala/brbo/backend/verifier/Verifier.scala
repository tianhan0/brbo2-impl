package brbo.backend.verifier

import brbo.common.{CommandLineArguments, MyLogger}
import brbo.common.ast.BrboProgram
import org.apache.logging.log4j.{LogManager, Logger}

abstract class Verifier {
  val commandLineArguments: CommandLineArguments
  val toolName: String
  val toolDirectory: String

  protected val logger: MyLogger = MyLogger(LogManager.getLogger(classOf[Verifier]), commandLineArguments.getDebugMode)

  def verify(program: BrboProgram): VerifierResult
}
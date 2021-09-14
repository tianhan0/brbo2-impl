package brbo.backend.verifier

import brbo.common.{CommandLineArguments, MyLogger}
import brbo.common.ast.BrboProgram
import org.apache.logging.log4j.{LogManager, Logger}

abstract class Verifier {
  val commandLineArguments: CommandLineArguments
  val toolName: String
  val toolDirectory: String

  protected val logger: Logger = LogManager.getLogger(classOf[Verifier])
  val myLogger: MyLogger = MyLogger(logger, commandLineArguments.getDebugMode)

  def verify(program: BrboProgram): VerifierResult
}
package brbo.backend.verifier

import brbo.common.ast.BrboProgram
import brbo.common.{CommandLineArguments, MyLogger}
import org.apache.logging.log4j.LogManager

abstract class Verifier {
  val arguments: CommandLineArguments
  val toolName: String
  val toolDirectory: String

  protected val logger: MyLogger = MyLogger(LogManager.getLogger(classOf[Verifier]), arguments.getDebugMode)

  def verify(program: BrboProgram): VerifierResult
}
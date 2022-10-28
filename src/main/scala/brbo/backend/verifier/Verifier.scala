package brbo.backend.verifier

import brbo.common.ast.BrboProgram
import brbo.common.{CommandLineArguments, MyLogger}
import org.apache.logging.log4j.LogManager

abstract class Verifier {
  val arguments: CommandLineArguments
  val toolName: String
  val toolDirectory: String
  val TIMEOUT: Int = arguments.getVerifierTimeout // Unit: Seconds

  protected val logger: MyLogger = MyLogger.createLogger(classOf[Verifier], arguments.getDebugMode)

  def verify(program: BrboProgram): VerifierResult
}
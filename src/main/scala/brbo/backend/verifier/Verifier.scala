package brbo.backend.verifier

import brbo.common.ast.BrboProgram
import brbo.common.MyLogger
import brbo.common.commandline.Arguments
import org.apache.logging.log4j.LogManager

abstract class Verifier {
  val arguments: Arguments
  val toolName: String
  val toolDirectory: String
  val TIMEOUT: Int = arguments.getVerifierTimeout // Unit: Seconds

  protected val logger: MyLogger = MyLogger.createLogger(classOf[Verifier], arguments.getDebugMode)

  def verify(program: BrboProgram): VerifierResult
}
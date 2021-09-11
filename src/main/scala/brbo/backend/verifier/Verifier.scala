package brbo.backend.verifier

import brbo.backend.verifier.VerifierRawResult.VerifierRawResult
import brbo.common.ast.BrboProgram
import org.apache.logging.log4j.{LogManager, Logger}

abstract class Verifier(val toolName: String, val toolPath: String) {
  protected val logger: Logger = LogManager.getLogger(classOf[Verifier])

  def verify(program: BrboProgram): VerifierResult
}

object VerifierRawResult extends Enumeration {
  type VerifierRawResult = Value
  val TRUE, FALSE, UNKNOWN = Value
}

case class VerifierResult(rawResult: VerifierRawResult, counterexamplePath: Option[CounterexamplePath])
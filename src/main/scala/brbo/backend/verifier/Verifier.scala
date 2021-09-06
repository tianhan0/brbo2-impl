package brbo.backend.verifier

import org.apache.logging.log4j.{LogManager, Logger}
import brbo.backend.verifier.VerifierRawResult.VerifierRawResult

abstract class Verifier(val toolName: String, val toolPath: String) {
  protected val logger: Logger = LogManager.getLogger(classOf[Verifier])

  def verify(program: String): VerifierResult
}

object VerifierRawResult extends Enumeration {
  type VerifierRawResult = Value
  val TRUE, FALSE, UNKNOWN = Value
}

case class VerifierResult(rawResult: VerifierRawResult, counterexamplePath: Option[CounterexamplePath])
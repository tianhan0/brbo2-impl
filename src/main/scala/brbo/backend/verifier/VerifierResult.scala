package brbo.backend.verifier

import brbo.backend.verifier.VerifierStatus._
import brbo.backend.verifier.cex.Path

case class VerifierResult(rawResult: VerifierStatus, counterexamplePath: Option[Path]) {
  rawResult match {
    case TRUE_RESULT | UNKNOWN_RESULT => assert(counterexamplePath.isEmpty)
    case FALSE_RESULT => assert(counterexamplePath.nonEmpty)
  }
}
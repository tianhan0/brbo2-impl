package brbo.backend.verifier

import brbo.backend.verifier.VerifierStatus._
import brbo.backend.verifier.cex.Path

case class VerifierResult(rawResult: VerifierStatus, counterexamplePaths: Set[Path]) {
  rawResult match {
    case TRUE_RESULT => assert(counterexamplePaths.isEmpty)
    case FALSE_RESULT => assert(counterexamplePaths.nonEmpty)
    case UNKNOWN_RESULT =>
  }
}
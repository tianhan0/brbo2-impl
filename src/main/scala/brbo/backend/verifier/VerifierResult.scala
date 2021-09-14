package brbo.backend.verifier

import brbo.backend.verifier.VerifierRawResult.VerifierRawResult
import brbo.backend.verifier.cex.Path

case class VerifierResult(rawResult: VerifierRawResult, counterexamplePath: Option[Path])
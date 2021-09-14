package brbo.backend.verifier

object VerifierRawResult extends Enumeration {
  type VerifierRawResult = Value
  val TRUE_RESULT, FALSE_RESULT, UNKNOWN_RESULT, UNINITIALIZED = Value
}

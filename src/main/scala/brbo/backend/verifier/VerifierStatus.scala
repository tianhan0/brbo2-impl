package brbo.backend.verifier

object VerifierStatus extends Enumeration with Serializable {
  type VerifierStatus = Value
  val TRUE_RESULT, FALSE_RESULT, UNKNOWN_RESULT = Value
}

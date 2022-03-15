package brbo.backend.verifier

object InterpreterKind extends Enumeration {
  type InterpreterKind = Value
  val SYMBOLIC_EXECUTION, MODEL_CHECK = Value
}

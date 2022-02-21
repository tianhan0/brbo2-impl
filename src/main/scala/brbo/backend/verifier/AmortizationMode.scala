package brbo.backend.verifier

object AmortizationMode extends Enumeration {
  type AmortizationMode = Value
  val NO_AMORTIZE, FULL_AMORTIZE, SELECTIVE_AMORTIZE, ALL_AMORTIZE, TEST_MODE = Value

  def amortizationModeToShortString(amortizationMode: AmortizationMode): String = {
    amortizationMode match {
      case NO_AMORTIZE => "noAmtz"
      case FULL_AMORTIZE => "fullAmtz"
      case SELECTIVE_AMORTIZE => "selAmtz"
      case ALL_AMORTIZE => "allAmtz"
      case TEST_MODE => "test"
    }
  }
}
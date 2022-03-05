package brbo.backend.verifier

object AmortizationMode extends Enumeration {
  type AmortizationMode = Value
  val NO_AMORTIZE, FULL_AMORTIZE, SELECTIVE_AMORTIZE, ALL_AMORTIZE, TEST_MODE = Value

  def amortizationModeToShortString(amortizationMode: AmortizationMode): String = {
    amortizationMode match {
      case NO_AMORTIZE => "noAmt"
      case FULL_AMORTIZE => "fullAmt"
      case SELECTIVE_AMORTIZE => "selAmt"
      case ALL_AMORTIZE => "allAmt"
      case TEST_MODE => "test"
    }
  }
}
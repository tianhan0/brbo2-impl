package brbo.common.ast

trait PrettyPrintToC {
  val DEFAULT_INDENT = 2

  def prettyPrintToC(indent: Int = 0): String
}

trait PrettyPrintPrintToCFG {
  def prettyPrintPrintToCFG: String
}
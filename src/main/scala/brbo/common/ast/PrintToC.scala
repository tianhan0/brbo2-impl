package brbo.common.ast

trait PrintToC {
  val DEFAULT_INDENT = 2

  def indentString(indent: Int): String = " " * indent

  def printToC(indent: Int): String
}

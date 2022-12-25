package brbo.common.ast

trait Print {
  val DEFAULT_INDENT = 2

  def indentString(indent: Int): String = " " * indent

  def printToC(indent: Int): String // TODO: We do not actually need this

  def printToBrboJava(indent: Int): String

  def printToQFuzzJava(indent: Int): String
}

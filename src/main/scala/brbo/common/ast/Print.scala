package brbo.common.ast

trait Print {
  val DEFAULT_INDENT = 2

  def indentString(indent: Int): String = " " * indent

  def print(indent: Int, style: PrintStyle.AbstractStyle): String
}

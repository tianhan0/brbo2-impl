package brbo.common.ast

trait OverrideToString {
  val DEFAULT_INDENT_IR = 2

  def toIR(indent: Int = 0): String

  override def toString: String = toIR()
}

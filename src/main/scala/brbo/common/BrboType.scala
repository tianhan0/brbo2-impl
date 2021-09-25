package brbo.common

object BrboType extends Enumeration {
  type BrboType = Value
  val INT, BOOL, VOID = Value

  def toCString(typ: BrboType): String = {
    typ match {
      case INT => "int"
      case BOOL => "int"
      case VOID => "void"
    }
  }
}
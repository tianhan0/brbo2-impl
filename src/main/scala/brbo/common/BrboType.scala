package brbo.common

object BrboType extends Enumeration {
  type BrboType = Value
  val INT, BOOL, VOID, STRING, FLOAT = Value

  def toCString(typ: BrboType): String = {
    typ match {
      case INT => "int"
      case BOOL => "int"
      case VOID => "void"
      case FLOAT => "float"
      case STRING => "$string$" // Intentionally invalid type name for C
    }
  }
}
package brbo.common

object BrboType {
  sealed trait T
  object INT extends T
  object BOOL extends T
  object VOID extends T
  object STRING extends T
  object FLOAT extends T
  case class ARRAY(typ: T) extends T

  def toCString(typ: T): String = {
    typ match {
      case INT => "int"
      case BOOL => "int"
      case VOID => "void"
      case FLOAT => "float"
      case STRING => "$string$" // Intentionally invalid type name for C
      case ARRAY(typ: T) => ???
    }
  }
}
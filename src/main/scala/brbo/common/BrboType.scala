package brbo.common

object BrboType {
  sealed trait T extends Serializable
  object INT extends T {
    override def toString: String = "INT"
  }
  object BOOL extends T {
    override def toString: String = "BOOL"
  }
  object VOID extends T {
    override def toString: String = "VOID"
  }
  object STRING extends T {
    override def toString: String = "STRING"
  }
  object FLOAT extends T {
    override def toString: String = "FLOAT"
  }
  case class ARRAY(typ: T) extends T {
    override def toString: String = s"ARRAY[${typ.toString}]"
  }

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
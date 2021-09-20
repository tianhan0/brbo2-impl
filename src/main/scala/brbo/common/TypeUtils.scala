package brbo.common

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT}
import org.checkerframework.framework.`type`.AnnotatedTypeMirror

import javax.lang.model.`type`.TypeMirror
import scala.collection.immutable.HashMap

object TypeUtils {
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

  def typeTranslation(typ: TypeMirror): BrboType = {
    val defaultTyp = INT
    typ.toString match {
      case "int" | "java.lang.Integer" => INT
      case "boolean" | "java.lang.Boolean" => BOOL
      case _ => defaultTyp
    }
  }

  def typeTranslation(anno: AnnotatedTypeMirror): BrboType = typeTranslation(anno.getUnderlyingType)

  def typeMapTranslation(map: Map[String, TypeMirror]): Map[String, BrboType] = {
    map.foldLeft(HashMap[String, BrboType]())({
      case (acc, (name, typ)) => acc + (name -> typeTranslation(typ))
    })
  }
}

package brbo.frontend

import brbo.common.BrboType.{BOOL, BrboType, INT}
import org.checkerframework.framework.`type`.AnnotatedTypeMirror

import javax.lang.model.`type`.TypeMirror
import scala.collection.immutable.HashMap

object TypeUtils {
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

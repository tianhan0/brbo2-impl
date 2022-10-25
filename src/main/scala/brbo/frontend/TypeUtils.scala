package brbo.frontend

import brbo.common.BrboType
import brbo.common.BrboType._
import org.checkerframework.framework.`type`.AnnotatedTypeMirror

import javax.lang.model.`type`.TypeMirror
import scala.collection.immutable.HashMap

object TypeUtils {
  def typeTranslation(typ: TypeMirror): BrboType.T = {
    typ.toString match {
      case "int" | "java.lang.Integer" => INT
      case "boolean" | "java.lang.Boolean" => BOOL
      case "java.lang.String" => STRING
      case "int[]" => ARRAY(INT)
      case "int[][]" => ARRAY(ARRAY(INT))
      case _ => throw new Exception(s"Translating unknown type `$typ`")
    }
  }

  def typeTranslation(anno: AnnotatedTypeMirror): BrboType.T = typeTranslation(anno.getUnderlyingType)

  def typeMapTranslation(map: Map[String, TypeMirror]): Map[String, BrboType.T] = {
    map.foldLeft(HashMap[String, BrboType.T]())({
      case (acc, (name, typ)) => acc + (name -> typeTranslation(typ))
    })
  }
}

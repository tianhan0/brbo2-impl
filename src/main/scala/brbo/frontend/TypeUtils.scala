package brbo.frontend

import brbo.common.BrboType
import brbo.common.BrboType.{BOOL, INT, STRING}
import org.checkerframework.framework.`type`.AnnotatedTypeMirror

import javax.lang.model.`type`.TypeMirror
import scala.collection.immutable.HashMap

object TypeUtils {
  def typeTranslation(typ: TypeMirror): BrboType.T = {
    typ.toString match {
      case "int" | "java.lang.Integer" => INT
      case "boolean" | "java.lang.Boolean" => BOOL
      case "java.lang.String" => STRING
      case _ => throw new Exception(s"Unknown type `$typ`")
    }
  }

  def typeTranslation(anno: AnnotatedTypeMirror): BrboType.T = typeTranslation(anno.getUnderlyingType)

  def typeMapTranslation(map: Map[String, TypeMirror]): Map[String, BrboType.T] = {
    map.foldLeft(HashMap[String, BrboType.T]())({
      case (acc, (name, typ)) => acc + (name -> typeTranslation(typ))
    })
  }
}

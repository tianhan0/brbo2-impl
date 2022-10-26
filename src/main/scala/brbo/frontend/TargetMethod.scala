package brbo.frontend

import brbo.common.BrboType._
import brbo.common.ast.Identifier
import brbo.common.{BrboType, MyLogger}
import com.sun.source.tree.{MethodTree, StatementTree, VariableTree}

import scala.collection.immutable.HashMap

case class TargetMethod(methodTree: MethodTree) {
  private val logger = MyLogger.createLogger(classOf[TargetMethod], debugMode = false)
  if (methodTree.getName == null) throw new Exception(s"Empty method name")

  JavaTreeUtils.acceptableTree(methodTree.getBody)

  val methodName: String = methodTree.getName.toString

  val returnType: BrboType.T =
    if (methodTree.getReturnType == null) VOID
    else {
      methodTree.getReturnType.toString match {
        case "int" => INT
        case "boolean" => BOOL
        case "void" => VOID
        case "int[]" => ARRAY(INT)
        case _ => throw new Exception(s"Unexpected return type: ${methodTree.getReturnType} (Kind: ${methodTree.getReturnType.getKind})")
      }
    }

  val inputVariables: Map[String, Identifier] = JavaTreeUtils.getAllInputVariables(methodTree).map({ case (name, typ) => (name, Identifier(name, typ)) })
  logger.trace(s"[Method `${methodTree.getName}`] Input variables: `$inputVariables`")

  val allCommands: Set[StatementTree] = JavaTreeUtils.collectCommands(methodTree.getBody)

  val localVariables: Map[String, Identifier] =
    if (methodTree.getBody == null) Map()
    else {
      allCommands.foldLeft(HashMap[String, BrboType.T]())({
        (acc, statement) =>
          statement match {
            case tree: VariableTree =>
              val name = tree.getName.toString
              val typ = org.checkerframework.javacutil.TreeUtils.typeOf(tree.getType)
              // Make sure that all variables in declared in a method have different names
              assert(!acc.keySet.contains(name), "Duplicate variable name: " + name)
              acc + (name -> TypeUtils.typeTranslation(typ))
            case _ => acc
          }
      }).map({ case (name, typ) => (name, Identifier(name, typ)) })
    }
  logger.trace(s"[Method `${methodTree.getName}`] Local variables: `$localVariables`")

  val variables: Map[String, Identifier] = inputVariables ++ localVariables
}
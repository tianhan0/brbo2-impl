package brbo.frontend

import brbo.common.BrboType
import brbo.frontend.TypeUtils.typeTranslation
import com.sun.source.tree._
import com.sun.source.util.TreePath

import javax.lang.model.`type`.TypeMirror
import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

object JavaTreeUtils {
  def acceptableTree(tree: StatementTree): Unit = {
    if (tree == null) return

    tree match {
      case _ if isCommand(tree) =>
        tree match {
          case variableTree: VariableTree => assert(variableTree.getInitializer != null, s"Variable declaration must have initializers: `$tree`")
          case _ =>
        }
      case blockTree: BlockTree => blockTree.getStatements.asScala.foreach(t => acceptableTree(t))
      case forLoopTree: ForLoopTree =>
        forLoopTree.getInitializer.asScala.foreach(t => acceptableTree(t))
        acceptableTree(forLoopTree.getStatement)
        forLoopTree.getUpdate.asScala.foreach(t => acceptableTree(t))
      case ifTree: IfTree =>
        acceptableTree(ifTree.getThenStatement)
        acceptableTree(ifTree.getElseStatement)
      // case labeledStatementTree: LabeledStatementTree => acceptableTree(labeledStatementTree.getStatement)
      case whileLoopTree: WhileLoopTree =>
        acceptableTree(whileLoopTree.getStatement)
      case _ => throw new Exception(s"Unsupported tree: `$tree`")
    }
  }

  def getAllInputVariables(methodTree: MethodTree): List[(String, BrboType.T)] = {
    methodTree.getParameters.asScala.foldLeft(Nil: List[(String, BrboType.T)])({
      (acc, param) =>
      val typ = org.checkerframework.javacutil.TreeUtils.typeOf(param.getType)
        (param.getName.toString, typeTranslation(typ)) :: acc
    }).reverse
  }

  def collectCommands(statement: StatementTree): Set[StatementTree] = {
    def throwException(message: String): Nothing = {
      throw new Exception(s"$message in AST: $statement")
    }

    if (statement == null) return HashSet()
    statement match {
      case _ if isCommand(statement) => HashSet(statement)
      case tree: BlockTree => collectCommands(tree.getStatements.asScala)
      case _: ClassTree => throwException("Unexpected class tree")
      case _: DoWhileLoopTree => throwException("Not yet support do while loop tree") // collectCommands(tree.getStatement)
      case _: EnhancedForLoopTree => throwException("Not yet support enhanced for tree")
      case tree: ForLoopTree =>
        val statements = tree.getInitializer.asScala.toList ::: tree.getStatement :: tree.getUpdate.asScala.toList
        collectCommands(statements)
      case tree: IfTree => collectCommands(tree.getThenStatement) ++ collectCommands(tree.getElseStatement)
      // case tree: LabeledStatementTree => collectCommands(tree.getStatement)
      case _: SwitchTree => throwException("Not yet support switch tree")
      case _: SynchronizedTree => throwException("Not yet support synchronized tree")
      case _: ThrowTree => throwException("Not yet support throw tree")
      case _: TryTree => throwException("Not yet support try tree")
      case tree: WhileLoopTree => collectCommands(tree.getStatement)
    }
  }

  def collectCommands(statements: Iterable[StatementTree]): Set[StatementTree] = {
    statements.foldLeft(HashSet[StatementTree]())({
      (acc, statement) => acc ++ collectCommands(statement)
    })
  }

  def isCommand(tree: StatementTree): Boolean = {
    if (tree == null) return false

    tree match {
      case _@(_: AssertTree | _: EmptyStatementTree | _: BreakTree | _: ContinueTree |
              _: ExpressionStatementTree | _: ReturnTree | _: VariableTree) => true
      case _ => false
    }
  }

  def getEnclosingTrees(path: TreePath): List[Tree] = {
    var enclosingTrees: List[Tree] = Nil
    var p = path
    while (p != null) {
      val leaf = p.getLeaf
      assert(leaf != null)
      enclosingTrees = leaf :: enclosingTrees
      p = p.getParentPath
    }
    assert(enclosingTrees.head.isInstanceOf[CompilationUnitTree])
    assert(enclosingTrees.tail.head.isInstanceOf[ClassTree])
    assert(enclosingTrees.tail.tail.head.isInstanceOf[MethodTree])
    enclosingTrees
  }

  def getEnclosingStatementTrees(path: TreePath): List[StatementTree] = {
    getEnclosingTrees(path).tail.tail.tail.map(t => t.asInstanceOf[StatementTree])
  }
}

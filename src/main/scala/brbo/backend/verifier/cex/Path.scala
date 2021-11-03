package brbo.backend.verifier.cex

import brbo.common.BrboType.BOOL
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{GhostVariableUtils, MyLogger}

case class Path(pathNodes: List[CFGNode]) {
  pathNodes.map(pathNode => pathNode.value).foreach({
    case Left(_) =>
    case Right(expr) => assert(expr.typ == BOOL)
  })

  val groupsInPath: Set[Int] = pathNodes.foldLeft(Set[Int]())({
    (acc, node) =>
      node.value match {
        case Left(command) =>
          command match {
            case Use(groupId, _, _, _) =>
              groupId match {
                case Some(value) => acc + value
                case None => acc
              }
            case Reset(groupId, _, _) => acc + groupId
            case _ => acc
          }
        case Right(_) => acc
      }
  })

  groupsInPath.foreach({
    groupId =>
      val (resource: Identifier, sharp: Identifier, counter: Identifier) = GhostVariableUtils.generateVariables(Some(groupId))
      // Every ghost variable that appears in the path must be initialized by a variable declaration
      assert(existDeclaration(resource), s"Resource variable `${resource.identifier}` is used but not declared!")
      assert(existDeclaration(sharp), s"Sharp variable `${sharp.identifier}` is used but not declared!")
      assert(existDeclaration(counter), s"Counter variable `${counter.identifier}` is used but not declared!")
  })

  override def toString: String = {
    val nodes = pathNodes.map({ node => node.toString }).mkString("\n  ")
    s"Path:\n  $nodes"
  }

  // Get segments of the given group in the given function
  def getSegments(groupId: Int, function: BrboFunction): List[Segment] = {
    val nodes = pathNodes.filter(node => node.function == function)
    if (nodes.isEmpty) return Nil
    var results: List[Segment] = Nil
    var begin = 0
    var end = 0
    var i = 1
    while (i < nodes.size) {
      if (nodes(i).isReset(Some(groupId), Some(function))) {
        results = Segment(this, begin, end) :: results
        begin = i
        end = i
      }
      else {
        end = end + 1
      }
      i = i + 1
    }
    results = Segment(this, begin, end) :: results
    assert(results.nonEmpty) // Every group has at least one segment, because of being initialized
    results.reverse
  }

  private def existDeclaration(identifier: Identifier): Boolean = {
    pathNodes.exists({
      node =>
        node.value match {
          case Left(command) =>
            command match {
              case VariableDeclaration(variable, _, _) => variable.sameAs(identifier)
              case _ => false
            }
          case Right(_) => false
        }
    })
  }
}

object Path {
  private val logger = MyLogger.createLogger(Path.getClass, debugMode = false)

  def removeCommandsForUBCheck(path: Option[Path]): Option[Path] = {
    path match {
      case Some(actualPath) =>
        val assertFunction: BrboFunction = PreDefinedFunctions.assert
        val nodes = actualPath.pathNodes
        var newNodes: List[CFGNode] = Nil
        var i = 0
        while (i < nodes.size) {
          val node = nodes(i)
          node.value match {
            case Left(command) =>
              command match {
                case CallFunction(callee, _) =>
                  if (callee == assertFunction) {
                    i = i + 1
                    val nextNode = nodes(i)
                    nextNode.value match {
                      case Left(_) => throw new Exception
                      case Right(condition) =>
                        condition match {
                          case Negative(Negative(cond, _), _) =>
                            assert(cond.prettyPrintToC() == "cond")
                            i = i + 2 // Directly exit
                          case Negative(cond, _) =>
                            assert(cond.prettyPrintToC() == "cond")
                            i = i + 3 // Reach the error location
                          case _ => throw new Exception
                        }
                    }
                  }
                  else {
                    newNodes = node :: newNodes
                    i = i + 1
                  }
                case _ =>
                  newNodes = node :: newNodes
                  i = i + 1
              }
            case Right(_) =>
              newNodes = node :: newNodes
              i = i + 1
          }
        }
        logger.error(s"Old path: $nodes")
        logger.error(s"New path: ${newNodes.reverse}")
        Some(Path(newNodes.reverse))
      case None => path
    }
  }
}

case class Segment(path: Path, begin: Int, end: Int)
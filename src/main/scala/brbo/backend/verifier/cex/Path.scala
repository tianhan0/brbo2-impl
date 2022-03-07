package brbo.backend.verifier.cex

import brbo.backend.refiner.PrintPath
import brbo.common.BrboType.BOOL
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{GhostVariableUtils, MyLogger}

case class Path(pathNodes: List[CFGNode]) {
  pathNodes.map(pathNode => pathNode.value).foreach({
    case _: Command =>
    case brboExpr: BrboExpr => assert(brboExpr.typ == BOOL)
  })

  val groupsInPath: Set[Int] = pathNodes.foldLeft(Set[Int]())({
    (acc, node) =>
      node.value match {
        case command: Command =>
          command match {
            case Use(groupId, _, _, _) =>
              groupId match {
                case Some(value) => acc + value
                case None => acc
              }
            case Reset(groupId, _, _) => acc + groupId
            case _ => acc
          }
        case _: BrboExpr => acc
      }
  })

  groupsInPath.foreach({
    groupId =>
      val (resource: Identifier, star: Identifier, counter: Identifier) = GhostVariableUtils.generateVariables(Some(groupId))
      // Every ghost variable that appears in the path must be initialized by a variable declaration
      assert(existDeclaration(resource), s"Resource variable `${resource.name}` is used but not declared!")
      assert(existDeclaration(star), s"Star variable `${star.name}` is used but not declared!")
      assert(existDeclaration(counter), s"Counter variable `${counter.name}` is used but not declared!")
  })

  override def toString: String = PrintPath.pathToString(pathNodes)

  // Get segments of the given group in the given function
  def getSegments(groupId: Int, functionName: String): List[Segment] = {
    val nodes = pathNodes.filter(node => node.functionIdentifier == functionName)
    if (nodes.isEmpty)
      return Nil
    var results: List[Segment] = Nil
    var begin = 0
    var end = 0
    var i = 1
    while (i < nodes.size) {
      if (nodes(i).isReset(Some(groupId), Some(functionName))) {
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
          case command: Command =>
            command match {
              case VariableDeclaration(variable, _, _) => variable.sameAs(identifier)
              case _ => false
            }
          case _: BrboExpr => false
        }
    })
  }
}

object Path {
  private val logger = MyLogger.createLogger(Path.getClass, debugMode = false)

  def removeCommandsForUBCheck(path: Path): Path = {
    val assertFunction: BrboFunction = PreDefinedFunctions.assert
    val nodes = path.pathNodes
    var newNodes: List[CFGNode] = Nil
    var i = 0
    while (i < nodes.size) {
      val node = nodes(i)
      node.value match {
        case command: Command =>
          command match {
            case BeforeFunctionCall(callee, _, _) =>
              if (callee.identifier == assertFunction.identifier) {
                i = i + 1
                val nextNode = nodes(i)
                logger.trace(s"nextNode: `$nextNode`")
                nextNode.value match {
                  case _: Command => throw new Exception
                  case condition: BrboExpr =>
                    condition match {
                      case Negation(Negation(cond, _), _) =>
                        assert(cond.prettyPrintToC() == "cond")
                        i = i + 2 // Directly exit
                      case Negation(cond, _) =>
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
        case _: BrboExpr =>
          newNodes = node :: newNodes
          i = i + 1
      }
    }
    logger.trace(s"Old path: $nodes")
    logger.trace(s"New path: ${newNodes.reverse}")
    Path(newNodes.reverse)
  }
}
package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{CommandLineArguments, MyLogger}

class PathRefinement(commandLineArguments: CommandLineArguments, brboProgram: BrboProgram) {
  private val maxGroups = commandLineArguments.getMaxGroups
  private val logger = MyLogger.createLogger(classOf[PathRefinement], commandLineArguments.getDebugMode)

  // Perform command transformations to commands in the given path
  def refine(path: Path): Set[Path] = {
    val useInsertedPaths: Set[Path] = insertUseOnly(path).toSet
    useInsertedPaths.flatMap({
      useInsertedPath: Path => removeResetOnly(useInsertedPath)
    })
  }

  def removeResetOnly(path: Path): Set[Path] = {
    def helper(numberToKeep: Int, currentPath: List[CFGNode], remaining: List[CFGNode]): Set[List[CFGNode]] = {
      assert(numberToKeep >= 0)
      remaining match {
        case Nil => Set(currentPath.reverse)
        case ::(head, tail) =>
          // Do not transform commands in functions other than the main function
          if (head.function.identifier != brboProgram.mainFunction.identifier)
            return helper(numberToKeep, head :: currentPath, tail)
          head.value match {
            case Left(command) =>
              command match {
                case Reset(_, _) =>
                  if (numberToKeep == 0) helper(numberToKeep, currentPath, tail)
                  else {
                    helper(numberToKeep - 1, head :: currentPath, tail) ++ helper(numberToKeep, currentPath, tail)
                  }
                case _ => helper(numberToKeep, head :: currentPath, tail)
              }
            case Right(_) => helper(numberToKeep, head :: currentPath, tail)
          }
      }
    }

    val numberOfResets =
      path.pathNodes.count({
        node =>
          node.value match {
            case Left(command) =>
              command match {
                case Reset(_, _) => true
                case _ => false
              }
            case Right(_) => false
          }
      })

    Range.inclusive(0, numberOfResets).toSet.flatMap({
      numberToKeep => helper(numberToKeep, Nil, path.pathNodes)
    }).map(newPath => Path(newPath))
  }

  def insertUseOnly(path: Path): List[Path] = {
    def helper(numberOfGroups: Int, currentPath: List[CFGNode], remaining: List[CFGNode]): List[List[CFGNode]] = {
      remaining match {
        case Nil => List(currentPath.reverse)
        case ::(head, tail) =>
          // Do not transform commands in functions other than the main function
          if (head.function.identifier != brboProgram.mainFunction.identifier)
            return helper(numberOfGroups, head :: currentPath, tail)
          head.value match {
            case Left(command) =>
              command match {
                case Use(_, update, _) =>
                  var splits: List[(Int, List[CFGNode])] = Nil
                  var numberOfNewGroups = 1
                  while (numberOfGroups + numberOfNewGroups <= maxGroups) {
                    splits = {
                      val newGroupIds = Range.inclusive(numberOfGroups + 1, numberOfGroups + numberOfNewGroups).toList.reverse
                      val newSplit = newGroupIds.flatMap({
                        i =>
                          // Rely on `head.id` to know which CFG edge the new nodes correspond to
                          val newUse = CFGNode(Left(Use(Some(i), update)), brboProgram.mainFunction, head.id)
                          val newReset = CFGNode(Left(Reset(i)), brboProgram.mainFunction, head.id)
                          List(newUse, newReset)
                      })
                      (numberOfNewGroups, newSplit) :: splits
                    }
                    numberOfNewGroups = numberOfNewGroups + 1
                  }
                  splits.flatMap({
                    case (numberOfNewGroups, newPrefix) => helper(numberOfNewGroups + numberOfGroups, newPrefix ::: currentPath, tail)
                  })
                case _ => helper(numberOfGroups, head :: currentPath, tail)
              }
            case Right(_) => helper(numberOfGroups, head :: currentPath, tail)
          }
      }
    }

    val numberOfGroups = {
      var groupIDs = Set[Int]()
      path.pathNodes.foreach({
        node =>
          node.value match {
            case Left(command) =>
              command match {
                case Use(groupID, _, _) => groupIDs = groupIDs + groupID.get
                case Reset(groupID, _) => groupIDs = groupIDs + groupID
                case _ =>
              }
            case Right(_) =>
          }
      })
      groupIDs.size
    }

    helper(numberOfGroups, Nil, path.pathNodes).map(newPath => Path(newPath))
  }
}

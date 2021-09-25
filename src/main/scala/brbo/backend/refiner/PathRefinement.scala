package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{CommandLineArguments, MathUtils, MyLogger}

class PathRefinement(commandLineArguments: CommandLineArguments, brboProgram: BrboProgram) {
  private val maxGroups = commandLineArguments.getMaxGroups
  private val logger = MyLogger.createLogger(classOf[PathRefinement], commandLineArguments.getDebugMode)

  // Perform command transformations to commands in the given path
  def refine(path: Path): List[Path] = {
    val useInsertedPaths: List[Path] = insertUseOnly(path)
    useInsertedPaths.flatMap({
      useInsertedPath: Path => insertResetOnly(useInsertedPath, 0, Nil)
    })
  }

  def insertResetOnly(path: Path, index: Int, choices: List[Int]): List[Path] = {
    // TODO: Restrict that, resets at every control location must be the same (Maybe this can be encoded in constraints???)
    val NOT_INSERT = -1
    if (index >= path.pathNodes.length) {
      // Instantiate the choices
      assert(path.pathNodes.length == choices.length)
      val reversed = path.pathNodes.zip(choices).foldLeft(Nil: List[CFGNode])({
        case (acc, (node, choice)) =>
          if (choice == NOT_INSERT) node :: acc
          else node :: CFGNode(Left(Reset(choice)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID) :: acc
      })
      return List(Path(reversed.reverse))
    }

    path.pathNodes(index).value match {
      case Left(command) =>
        command match {
          case Use(groupID, _, _, _) =>
            val notInsertResetBeforeThisUse = insertResetOnly(path, index + 1, choices :+ NOT_INSERT)
            val insertResetBeforeThisUse = insertResetOnly(path, index + 1, choices :+ groupID.get)
            notInsertResetBeforeThisUse ::: insertResetBeforeThisUse
          case _ => insertResetOnly(path, index + 1, choices :+ NOT_INSERT)
        }
      case Right(_) => insertResetOnly(path, index + 1, choices :+ NOT_INSERT)
    }
  }

  def insertUseOnly(path: Path): List[Path] = {
    val nodes = path.pathNodes
    val numberOfUses = nodes.count({
      node =>
        node.value match {
          case Left(command) => command.isInstanceOf[Use]
          case Right(_) => false
        }
    })
    val results: Set[List[CFGNode]] = MathUtils.generateUniqueSequences(numberOfUses, maxGroups).map({
      transformation =>
        var i = 0
        var j = 0 // Index of use
        var result: List[CFGNode] = Nil
        while (i < nodes.length) {
          val newNode: CFGNode = {
            val unchanged = nodes(i)
            nodes(i).value match {
              case Left(command) =>
                command match {
                  case Use(_, update, assignment, _) =>
                    // Do not transform commands in functions other than the main function
                    val newNode: CFGNode =
                      if (nodes(i).function.identifier == brboProgram.mainFunction.identifier) {
                        // Transform according to the renaming scheme
                        CFGNode(Left(Use(Some(transformation(j)), update, assignment)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
                      }
                      else unchanged
                    j = j + 1
                    newNode
                  case _ => unchanged
                }
              case Right(_) => unchanged
            }
          }
          result = newNode :: result
          i = i + 1
        }
        result.reverse
    })
    results.map(list => Path(list)).toList
  }
}

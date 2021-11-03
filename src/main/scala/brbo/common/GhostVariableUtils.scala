package brbo.common

import brbo.common.BrboType.INT
import brbo.common.GhostVariableTyp._
import brbo.common.ast._

object GhostVariableUtils {
  private val logger = MyLogger.createLogger(GhostVariableUtils.getClass, debugMode = false)

  private val resourceVariablePrefix = "R"
  private val sharpVariablePrefix = "S"
  private val counterVariablePrefix = "C"
  private val resourceVariablePattern = (resourceVariablePrefix + """\d*""").r
  private val sharpVariablePattern = (sharpVariablePrefix + """\d*""").r
  private val counterVariablePattern = (counterVariablePrefix + """\d*""").r

  def generateVariable(groupId: Option[Int], typ: GhostVariable): Identifier = {
    groupId match {
      case Some(i) => Identifier(GhostVariableUtils.generateName(i.toString, typ), INT)
      case None => Identifier(GhostVariableUtils.generateName("", typ), INT)
    }
  }

  def generateVariables(groupId: Option[Int]): (Identifier, Identifier, Identifier) = {
    val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource)
    val sharpVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Sharp)
    val counterVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Counter)
    (resourceVariable, sharpVariable, counterVariable)
  }

  def declareVariables(groupId: Int): List[Command] = {
    val (resource: Identifier, sharp: Identifier, counter: Identifier) = GhostVariableUtils.generateVariables(Some(groupId))
    val declaration1 = VariableDeclaration(resource, Number(0))
    val declaration2 = VariableDeclaration(sharp, Number(0))
    val declaration3 = VariableDeclaration(counter, Number(-1))
    List(declaration1, declaration2, declaration3)
  }

  private def generateName(suffix: String, typ: GhostVariable): String = {
    val result = typ match {
      case Resource => s"$resourceVariablePrefix$suffix"
      case Sharp => s"$sharpVariablePrefix$suffix"
      case Counter => s"$counterVariablePrefix$suffix"
    }
    assert(isGhostVariable(result, typ))
    result
  }

  def isGhostVariable(identifier: String): Boolean = {
    isGhostVariable(identifier, Resource) || isGhostVariable(identifier, Sharp) || isGhostVariable(identifier, Counter)
  }

  def isGhostVariable(identifier: String, typ: GhostVariable): Boolean = {
    val pattern = typ match {
      case Resource => resourceVariablePattern
      case Sharp => sharpVariablePattern
      case Counter => counterVariablePattern
    }
    identifier match {
      case pattern() => true
      case _ => false
    }
  }

  // To successfully extract, the assignment must be in the form of `R = R + e`
  /*def extractUseFromAssignment(cfgNode: Either[Command, BrboExpr]): Option[Use] = {
    cfgNode match {
      case Left(command) =>
        command match {
          case assignment@Assignment(variable, expression, _) =>
            if (variable.identifier == resourceVariablePrefix) {
              expression match {
                case Addition(left, right, _) =>
                  left match {
                    case Identifier(identifier, _, _) =>
                      if (identifier == variable.identifier) Some(Use(None, right))
                      else None
                    case _ => None
                  }
                case _ => None
              }
            }
            else None
          case _ => None
        }
      case Right(_) => None
    }
  }*/
}

package brbo.common

import brbo.common.BrboType.INT
import brbo.common.GhostVariableTyp._
import brbo.common.ast._

object GhostVariableUtils {
  private val logger = MyLogger.createLogger(GhostVariableUtils.getClass, debugMode = false)

  private val resourceVariablePrefix = "R"
  private val starVariablePrefix = "D"
  private val counterVariablePrefix = "C"
  private val resourceVariablePattern = (resourceVariablePrefix + """\d*""").r
  private val starVariablePattern = (starVariablePrefix + """\d*""").r
  private val counterVariablePattern = (counterVariablePrefix + """\d*""").r
  val resourceInitialValue: Int = 0
  val starInitialValue: Int = Int.MinValue
  val counterInitialValue: Int = 0

  def initialValue(identifier: String): Int = {
    if (isGhostVariable(identifier, Resource))
      resourceInitialValue
    else if (isGhostVariable(identifier, Star))
      starInitialValue
    else if (isGhostVariable(identifier, Counter))
      counterInitialValue
    else throw new Exception
  }

  def approximatedResourceUsage(groupIds: Set[Int]): BrboExpr = groupIds.toList.sorted.foldLeft(Number(0): BrboExpr)({
    (acc, groupId) => Addition(acc, generateSum(Some(groupId)))
  })

  def generateVariable(groupId: Option[Int], typ: GhostVariableTyp): Identifier = {
    groupId match {
      case Some(i) => Identifier(GhostVariableUtils.generateName(i.toString, typ), INT)
      case None => Identifier(GhostVariableUtils.generateName("", typ), INT)
    }
  }

  def generateVariables(groupId: Option[Int]): (Identifier, Identifier, Identifier) = {
    val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource)
    val starVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Star)
    val counterVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Counter)
    (resourceVariable, starVariable, counterVariable)
  }

  def generateSum(groupId: Option[Int]): BrboExpr = {
    val (resourceVariable, starVariable, counterVariable) = generateVariables(groupId)
    Addition(resourceVariable, Multiplication(starVariable, counterVariable))
  }

  def declareVariables(groupId: Int): List[Command] = {
    val (resource: Identifier, star: Identifier, counter: Identifier) = GhostVariableUtils.generateVariables(Some(groupId))
    val declaration1 = VariableDeclaration(resource, Number(initialValue(resource.name)))
    val declaration2 = VariableDeclaration(star, Number(initialValue(star.name)))
    val declaration3 = VariableDeclaration(counter, Number(initialValue(counter.name)))
    List(declaration1, declaration2, declaration3)
  }

  private def generateName(suffix: String, typ: GhostVariableTyp): String = {
    val result = typ match {
      case Resource => s"$resourceVariablePrefix$suffix"
      case Star => s"$starVariablePrefix$suffix"
      case Counter => s"$counterVariablePrefix$suffix"
    }
    assert(isGhostVariable(result, typ))
    result
  }

  def isGhostVariable(identifier: String): Boolean = {
    isGhostVariable(identifier, Resource) || isGhostVariable(identifier, Star) || isGhostVariable(identifier, Counter)
  }

  def isGhostVariable(identifier: String, typ: GhostVariableTyp): Boolean = {
    val pattern = typ match {
      case Resource => resourceVariablePattern
      case Star => starVariablePattern
      case Counter => counterVariablePattern
    }
    identifier match {
      case pattern() => true
      case _ => false
    }
  }

  def getId(identifier: String, typ: GhostVariableTyp): Option[Int] = {
    val pattern = typ match {
      case Resource => resourceVariablePattern
      case Star => starVariablePattern
      case Counter => counterVariablePattern
    }
    identifier match {
      case pattern(id) => Some(id.toInt)
      case _ => None
    }
  }
}

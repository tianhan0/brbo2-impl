package brbo.common

import brbo.common.BrboType.INT
import brbo.common.GhostVariableTyp._
import brbo.common.ast._

object GhostVariableUtils {
  private val logger = MyLogger.createLogger(GhostVariableUtils.getClass, debugMode = false)

  private val resourceVariablePrefix = "R"
  private val starVariablePrefix = "S"
  private val counterVariablePrefix = "C"
  private val resourceVariablePattern = (resourceVariablePrefix + """\d*""").r
  private val starVariablePattern = (starVariablePrefix + """\d*""").r
  private val counterVariablePattern = (counterVariablePrefix + """\d*""").r
  val resourceInitialValue: Int = 0
  val starInitialValue: Int = Int.MinValue
  val counterInitialValue: Int = 0

  def initialValue(typ: GhostVariableTyp): Int = {
    typ match {
      case Resource => resourceInitialValue
      case Star => starInitialValue
      case Counter => counterInitialValue
    }
  }

  def approximatedResourceUsage(groupIds: Set[Int],
                                legacy: Boolean
                               ): BrboExpr = {
    groupIds.toList.sorted.foldLeft(Number(0): BrboExpr)({
      (acc, groupId) => Addition(acc, generateSum(groupId = Some(groupId), legacy = legacy))
    })
  }

  def generateVariable(groupId: Option[Int], typ: GhostVariableTyp, legacy: Boolean = false): Identifier = {
    groupId match {
      case Some(i) => Identifier(GhostVariableUtils.generateName(i.toString, typ, legacy), INT)
      case None => Identifier(GhostVariableUtils.generateName("", typ, legacy), INT)
    }
  }

  def generateVariables(groupId: Option[Int], legacy: Boolean): (Identifier, Identifier, Identifier) = {
    val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource, legacy)
    val starVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Star, legacy)
    val counterVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Counter, legacy)
    (resourceVariable, starVariable, counterVariable)
  }

  def generateSum(groupId: Option[Int], legacy: Boolean): BrboExpr = {
    val (resourceVariable, starVariable, counterVariable) = generateVariables(groupId, legacy)
    val actualCounter =
      if (legacy) Subtraction(counterVariable, Number(1))
      else counterVariable
    Addition(
      resourceVariable,
      Multiplication(starVariable, actualCounter)
    )
  }

  def declareVariables(groupId: Int, legacy: Boolean): List[Command] = {
    val (resource: Identifier, star: Identifier, counter: Identifier) = GhostVariableUtils.generateVariables(Some(groupId), legacy)
    val declaration1 = VariableDeclaration(resource, Number(initialValue(Resource)))
    val declaration2 = VariableDeclaration(star, Number(if (legacy) 0 else initialValue(Star)))
    val declaration3 = VariableDeclaration(counter, Number(if (legacy) -1 else initialValue(Counter)))
    List(declaration1, declaration2, declaration3)
  }

  private def generateName(suffix: String, typ: GhostVariableTyp, legacy: Boolean): String = {
    val result = typ match {
      case Resource => if (legacy) s"D$suffix" else s"$resourceVariablePrefix$suffix"
      case Star => if (legacy) s"D${suffix}p" else s"$starVariablePrefix$suffix"
      case Counter => s"$counterVariablePrefix$suffix"
    }
    // assert(isGhostVariable(result, typ))
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

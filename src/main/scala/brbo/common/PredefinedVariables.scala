package brbo.common

object PredefinedVariables {
  val MAX = 8
  val LARGE_INT = 10000000

  val variableNames: List[String] = List("MAX", "LARGE_INT").sorted

  val variables: Map[String, Int] = Map(
    "MAX" -> MAX,
    "LARGE_INT" -> LARGE_INT,
  )
}

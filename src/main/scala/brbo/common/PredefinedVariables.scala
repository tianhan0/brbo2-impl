package brbo.common

import brbo.backend2.qfuzz.DriverGenerator

object PredefinedVariables {
  val MAX = 8
  val LARGE_INT = 10000000
  val variables: Map[String, Int] = Map(
    "MAX" -> MAX,
    "LARGE_INT" -> LARGE_INT,
    // "BOOLEAN_SEPARATOR" -> (DriverGenerator.MAX_INTEGER + DriverGenerator.MIN_INTEGER) / 2,
  )
}

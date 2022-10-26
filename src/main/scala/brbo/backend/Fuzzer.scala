package brbo.backend

import brbo.common.ast._
import brbo.common.{BrboType, MathUtils, MyLogger}

object Fuzzer {
  private val logger = MyLogger.createLogger(Fuzzer.getClass, debugMode = false)

  def randomValues(typ: BrboType.T, maxArrayLength: Int, maxInteger: Int, possibilities: Int): List[BrboExpr] = {
    typ match {
      case BrboType.INT =>
        val random = new scala.util.Random
        Range.inclusive(0, possibilities).toList.map(_ => Number(random.nextInt(maxInteger)))
      case BrboType.BOOL => List(Bool(b = false), Bool(b = true))
      case BrboType.ARRAY(typ) =>
        typ match {
          case BrboType.INT =>
            Range.inclusive(0, maxArrayLength).toList.flatMap({
              length =>
                MathUtils.crossJoin(
                  Range(0, length)
                    .toList
                    .map({ _ => randomValues(BrboType.INT, maxArrayLength, maxInteger, possibilities) })
                ).map(list => BrboArray(list, BrboType.INT))
            })
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}

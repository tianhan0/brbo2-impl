package brbo.backend.verifier.cex

import brbo.TestCase
import org.scalatest.flatspec.AnyFlatSpec

class MiniCParserUnitTest extends AnyFlatSpec {

}

object MiniCParserUnitTest {
  val testCases: List[TestCase] = {
    val test01 =
      """int i = 0;
        |int R = 0;
        |[!(!(cond))]
        |return __VERIFIER_nondet_int();
        |int x = ndInt();
        |[!(!(cond))]
        |return x;
        |[i < n]
        |int e = 0;
        |[i < 1]
        |e = (a)
        |R = (R + e)
        |i = (i + 1)
        |[i < n]
        |int e = 0;
        |[!(i < 1)]
        |e = (b)
        |R = (R + e)
        |i = (i + 1)
        |[!(i < n)]
        |[!(cond)]
        |__VERIFIER_error()""".stripMargin
    val test01Expected = """"""

    List(TestCase("Test 1", test01, test01Expected))
  }
}

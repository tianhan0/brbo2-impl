package brbo.backend.interpreter

import org.scalatest.flatspec.AnyFlatSpec

class InterpreterUnitTest extends AnyFlatSpec {

}

object InterpreterUnitTest {
  val program = """class Test {
                  |  void main(int n) {
                  |    int i = 0;
                  |  }
                  |}""".stripMargin
}
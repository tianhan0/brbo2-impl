package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTrace, Trace}
import brbo.backend2.learning.ScriptRunner.{Algorithm, Optics, Precomputed}
import brbo.backend2.learning.{Clustering, TraceClustering}
import brbo.common.ast.{BoundAssertion, BrboProgram}
import brbo.common.{CommandLineArguments, MyLogger}

class Driver(arguments: CommandLineArguments, program: BrboProgram) {
  private val debugMode = arguments.getDebugMode
  private val logger = MyLogger.createLogger(classOf[Driver], debugMode)

  def verify(boundAssertion: BoundAssertion): Unit = {
    val rawTraces = generateTraces()
  }

  def generateTraces(): List[Interpreter.Trace] = {
    logger.info(s"Step 1: Generate traces")
    Fuzzer.fuzz(program, debugMode)
  }
}

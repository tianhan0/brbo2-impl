package brbo.backend.verifier

import brbo.backend.verifier.VerifierRawResult.VerifierRawResult
import brbo.common.FileUtils
import brbo.common.ast.BrboProgram

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, TimeoutException, blocking}
import scala.sys.process.{ProcessLogger, _}

class UAutomizerVerifier extends Verifier(toolName = "UAutomizer", toolPath = s"${System.getProperty("user.home")}/Documents/workspace/UAutomizer-linux") {
  private val VIOLATION_WITNESS = s"$toolPath/witness.graphml"
  private val TIMEOUT = 60 // Unit: Seconds

  override def verify(program: BrboProgram): VerifierResult = {
    val result: VerifierRawResult =
      runAndGetStdOutput(program.prettyPrintToC(indent = 0)) match {
        case Some(output) =>
          if (output.endsWith("Result:FALSE")) VerifierRawResult.FALSE
          else if (output.endsWith("Result:TRUE")) VerifierRawResult.TRUE
          else if (output.endsWith("Result:UNKNOWN")) VerifierRawResult.UNKNOWN
          else {
            logger.error(s"Cannot interpret results from `$toolName`:\n$output")
            throw new Exception
          }
        case None => VerifierRawResult.UNKNOWN
      }

    result match {
      case VerifierRawResult.FALSE =>
        val violationWitnessFile = new File(VIOLATION_WITNESS)
        assert(violationWitnessFile.exists())

        val counterexamplePath: String = FileUtils.readFromFile(VIOLATION_WITNESS)
        VerifierResult(result, Some(CounterexamplePath.graphMLToCounterexamplePath(counterexamplePath, program.mainFunction)))
      case VerifierRawResult.TRUE | VerifierRawResult.UNKNOWN => VerifierResult(result, None)
    }
  }

  private def runAndGetStdOutput(sourceCode: String): Option[String] = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val file = Files.createTempFile("prefix-", ".c")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(sourceCode)
      close()
    }

    try {
      val deleteCounterexampleFile = s"rm $VIOLATION_WITNESS"
      logger.trace(s"Delete violation witness file: `$deleteCounterexampleFile`")
      deleteCounterexampleFile.run(ProcessLogger(stdout append _, stderr append _))

      val runVerifier = s"$toolPath/Ultimate.py --spec $toolPath/unreach-call.prp --architecture 64bit --file ${file.toAbsolutePath} --witness-type violation_witness"
      val process = runVerifier.run(ProcessLogger(stdout append _, stderr append _))
      logger.trace(s"Run `$toolName`: `$runVerifier`")

      val future = Future(blocking(process.exitValue())) // wrap in Future
      val actualTimeout = {
        if (TIMEOUT >= 0) Duration(TIMEOUT, SECONDS)
        else Duration.Inf
      }
      val result =
        try {
          Await.result(future, actualTimeout)
        } catch {
          case _: TimeoutException =>
            logger.fatal(s"`$toolName` timed out after `$actualTimeout`!")
            process.destroy()
            process.exitValue()
        }
      if (result == 0) {
        logger.trace(s"`$toolName` stdout:\n$stdout")
        val removeFile = s"rm $file"
        removeFile.!!
        Some(stdout.toString())
      }
      else {
        logger.fatal(s"Error when running `$toolName`. Exit code: `$result`")
        logger.fatal(s"stderr:\n$stderr")
        None
      }
    }
    catch {
      case e: Exception =>
        logger.fatal(s"Exception!", e)
        logger.fatal(s"stdout:\n$stdout")
        logger.fatal(s"stderr:\n$stderr")
        throw new RuntimeException(s"Error when running `$toolName`")
    }
  }
}

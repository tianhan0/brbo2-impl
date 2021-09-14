package brbo.backend.verifier

import brbo.backend.verifier.VerifierRawResult.VerifierRawResult
import brbo.backend.verifier.cex.ParseCounterexamplePath
import brbo.common.ast.BrboProgram
import brbo.common.{CommandLineArguments, FileUtils}

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, TimeoutException, blocking}
import scala.sys.process.{ProcessLogger, _}

class UAutomizerVerifier(override val commandLineArguments: CommandLineArguments) extends Verifier {
  override val toolName = "UAutomizer"
  override val toolDirectory: String = commandLineArguments.getModelCheckerDirectory
  private val TIMEOUT = commandLineArguments.getModelCheckerTimeout // Unit: Seconds

  private val VIOLATION_WITNESS = s"$toolDirectory/witness.graphml"
  private val PROPERTY_FILE = s"$toolDirectory/unreach-call.prp"
  private val EXECUTABLE = s"$toolDirectory/Ultimate.py"

  override def verify(program: BrboProgram): VerifierResult = {
    val result: VerifierRawResult = {
      val cSourceCode = program.prettyPrintToC()
      logger.debug(s"Input to UAutomizer:\n$cSourceCode")
      runAndGetStdOutput(cSourceCode) match {
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
    }

    result match {
      case VerifierRawResult.FALSE =>
        val violationWitnessFile = new File(VIOLATION_WITNESS)
        assert(violationWitnessFile.exists())

        val counterexamplePath: String = FileUtils.readFromFile(VIOLATION_WITNESS)
        val parseCounterexamplePath = new ParseCounterexamplePath(commandLineArguments.getDebugMode)
        VerifierResult(result, Some(parseCounterexamplePath.graphMLToCounterexamplePath(counterexamplePath, program)))
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

      val runVerifier = s"$EXECUTABLE --spec $PROPERTY_FILE --architecture 64bit --file ${file.toAbsolutePath} --witness-type violation_witness"
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

object UAutomizerVerifier {
  val TOOL_DIRECTORY = s"${System.getProperty("user.home")}/Documents/workspace/UAutomizer-linux"
}
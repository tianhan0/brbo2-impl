package brbo.backend.verifier

import brbo.backend.verifier.cex.ParseCounterexamplePath
import brbo.common.ast.{BrboProgram, BrboProgramInC}
import brbo.common.{CommandLineArguments, FileUtils}

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, TimeoutException, blocking}
import scala.sys.process.{ProcessLogger, _}

class UAutomizerVerifier(override val arguments: CommandLineArguments) extends Verifier {
  override val toolName = "UAutomizer"
  override val toolDirectory: String = arguments.getModelCheckerDirectory

  private val TIMEOUT = arguments.getModelCheckerTimeout // Unit: Seconds
  private val PROPERTY_FILE = s"$toolDirectory/unreach-call.prp"
  private val EXECUTABLE_FILE = s"./Ultimate.py"
  private val FULL_OUTPUT = "--full-output"

  override def verify(program: BrboProgram): VerifierResult = {
    val programInC = BrboProgramInC(program)

    val cSourceCode = programInC.program.prettyPrintToC()
    logger.traceOrError(s"Input to UAutomizer:\n$cSourceCode")
    runAndGetStdOutput(cSourceCode) match {
      case (Some(output), violationWitnessFile) =>
        if (output.endsWith("Result:FALSE")) {
          val counterexamplePath: String = FileUtils.readFromFile(violationWitnessFile.toAbsolutePath.toString)
          val parseCounterexamplePath = new ParseCounterexamplePath(arguments.getDebugMode)
          val pathInC = parseCounterexamplePath.graphMLToCounterexamplePath(counterexamplePath, programInC.program)

          val removeFile = s"rm ${violationWitnessFile.toAbsolutePath.toString}"
          removeFile.!!

          VerifierResult(VerifierRawResult.FALSE_RESULT, Some(parseCounterexamplePath.extractUseResetFromCRepresentation(pathInC, programInC)))
        }
        else if (output.endsWith("Result:TRUE")) {
          VerifierResult(VerifierRawResult.TRUE_RESULT, None)
        }
        else if (output.endsWith("Result:UNKNOWN")) {
          VerifierResult(VerifierRawResult.UNKNOWN_RESULT, None)
        }
        else {
          logger.error(s"Cannot interpret results from `$toolName`:\n$output")
          throw new Exception
        }
      case _ => VerifierResult(VerifierRawResult.UNKNOWN_RESULT, None)
    }
  }

  private def runAndGetStdOutput(sourceCode: String): (Option[String], Path) = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val file = Files.createTempFile("prefix-", ".c")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(sourceCode)
      close()
    }

    try {
      // Use option `--witness-name` to randomize the name of this file
      val violationWitnessFile = Files.createTempFile(Paths.get(toolDirectory),"witness-", ".graphml")

      // logger.infoOrError(s"Please put binary file `mathsat` (provided by Ultimate) under directory `/usr/bin`")
      // E.g., `~/Documents/workspace/UAutomizer-linux$ ./Ultimate.py --spec unreach-call.prp --architecture 64bit --file ~/win_c/Desktop/test.c --witness-type violation_witness`
      val command = s"$EXECUTABLE_FILE --spec $PROPERTY_FILE --architecture 64bit --file ${file.toAbsolutePath} --witness-type violation_witness --witness-name ${violationWitnessFile.toAbsolutePath.toString}"
      // Set the working directory for the command
      val processBuilder = sys.process.Process(command, new java.io.File(toolDirectory))
      val process = processBuilder.run(ProcessLogger(stdout append _, stderr append _))
      logger.traceOrError(s"Run `$toolName` via command `$command`")

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
        logger.traceOrError(s"stdout:\n$stdout")
        val removeFile = s"rm $file"
        removeFile.!!
        (Some(stdout.toString()), violationWitnessFile)
      }
      else {
        logger.fatal(s"Error when running `$toolName`. Exit code: `$result`")
        logger.fatal(s"stdout:\n$stdout")
        logger.fatal(s"stderr:\n$stderr")
        (None, violationWitnessFile)
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
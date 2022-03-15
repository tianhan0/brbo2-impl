package brbo.backend.verifier

import brbo.backend.verifier.cex.ParseCounterexamplePath
import brbo.common.ast.{BrboProgram, BrboProgramInC}
import brbo.common.{CommandLineArguments, FileUtils}
import org.apache.commons.io.IOUtils

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, SECONDS}
import scala.sys.process._

class UAutomizerVerifier(override val arguments: CommandLineArguments) extends Verifier {
  override val toolName = "UAutomizer"
  override val toolDirectory: String = arguments.getVerifierDirectory

  private val PROPERTY_FILE = s"$toolDirectory/unreach-call.prp"
  private val EXECUTABLE_FILE = s"./Ultimate.py"
  private val FULL_OUTPUT = "--full-output"

  override def verify(program: BrboProgram): VerifierResult = {
    val programInC = BrboProgramInC(program)

    val cSourceCode = programInC.program.prettyPrintToC()
    if (arguments.getPrintVerifierInputs) logger.info(s"Input to UAutomizer:\n$cSourceCode")
    else logger.traceOrError(s"Input to UAutomizer:\n$cSourceCode")
    runAndGetStdOutput(cSourceCode) match {
      case (Some(output), violationWitnessFile) =>
        val outputTrimmed = output.replaceAll("\\s", "")
        if (outputTrimmed.endsWith("Result:FALSE")) {
          val counterexamplePath: String = FileUtils.readFromFile(violationWitnessFile.toAbsolutePath.toString)
          val parseCounterexamplePath = new ParseCounterexamplePath(arguments.getDebugMode)
          val pathInC = parseCounterexamplePath.graphMLToCounterexamplePath(counterexamplePath, programInC.program)

          val removeFile = s"rm ${violationWitnessFile.toAbsolutePath.toString}"
          removeFile.!!

          VerifierResult(VerifierStatus.FALSE_RESULT, Set(parseCounterexamplePath.extractUseResetFromCRepresentation(pathInC, programInC)))
        }
        else if (outputTrimmed.endsWith("Result:TRUE")) {
          VerifierResult(VerifierStatus.TRUE_RESULT, Set())
        }
        else if (outputTrimmed.endsWith("Result:UNKNOWN")) {
          VerifierResult(VerifierStatus.UNKNOWN_RESULT, Set())
        }
        else {
          logger.error(s"Cannot interpret results from `$toolName`:\n$outputTrimmed")
          throw new Exception
        }
      case _ => VerifierResult(VerifierStatus.UNKNOWN_RESULT, Set())
    }
  }

  private def runAndGetStdOutput(sourceCode: String): (Option[String], Path) = {
    val stdout = new StringBuilder

    val file = Files.createTempFile("prefix-", ".c")
    new PrintWriter(file.toAbsolutePath.toString) {
      write(sourceCode)
      close()
    }

    try {
      // Use option `--witness-name` to randomize the name of this file
      val violationWitnessFile = Files.createTempFile(Paths.get(toolDirectory), "witness-", ".graphml")

      // logger.infoOrError(s"Please put binary file `mathsat` (provided by Ultimate) under directory `/usr/bin`")
      // E.g., `~/Documents/workspace/UAutomizer-linux$ ./Ultimate.py --spec unreach-call.prp --architecture 64bit --file ~/win_c/Desktop/test.c --witness-type violation_witness`
      val command = s"$EXECUTABLE_FILE --spec $PROPERTY_FILE --architecture 64bit --file ${file.toAbsolutePath} --witness-type violation_witness --witness-name ${violationWitnessFile.toAbsolutePath.toString}"
      // val processBuilder = sys.process.Process(command, new java.io.File(toolDirectory))
      // val process: Process = processBuilder.run(ProcessLogger(stdout append _, stderr append _))
      // Set the working directory for the command
      val processBuilder: java.lang.ProcessBuilder = new java.lang.ProcessBuilder(command.split(" ").toList.asJava)
        .directory(new java.io.File(toolDirectory))
        .redirectErrorStream(true)
      val process: java.lang.Process = processBuilder.start()
      if (arguments.getPrintVerifierInputs) logger.info(s"Run `$toolName` via command `$command`")
      else logger.traceOrError(s"Run `$toolName` via command `$command`")

      val actualTimeout = {
        if (TIMEOUT >= 0) Duration(TIMEOUT, SECONDS)
        else Duration.Inf
      }
      val result: Int = {
        if (!process.waitFor(actualTimeout.toSeconds, TimeUnit.SECONDS)) {
          logger.fatal(s"`$toolName` timed out after `$actualTimeout`!")
          process.descendants().forEach({
            handle =>
              logger.traceOrError(s"Kill descendant process `${handle.pid()}`")
              handle.destroy()
          })
          logger.traceOrError(s"Kill current process `${process.pid()}`")
          process.destroy(); // consider using destroyForcibly instead
          -1
        }
        else process.exitValue()
      }
      val stdout = {
        try {
          IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
        }
        catch {
          case _: Exception => "stdout is empty (Since the process timed out)"
        }
      }
      if (result == 0) {
        logger.traceOrError(s"stdout:\n$stdout")
        val removeFile = s"rm $file"
        removeFile.!!
        (Some(stdout), violationWitnessFile)
      }
      else {
        logger.fatal(s"Error when running `$toolName`. Exit code: `$result`")
        logger.fatal(s"stdout:\n$stdout")
        (None, violationWitnessFile)
      }
    }
    catch {
      case e: Exception =>
        logger.fatal(s"Exception!", e)
        throw new RuntimeException(s"Error when running `$toolName`")
    }
  }
}

object UAutomizerVerifier {
  val TOOL_DIRECTORY = s"${System.getProperty("user.home")}/Documents/workspace/UAutomizer-linux"
}
package brbo.backend2.qfuzz

import brbo.BrboMain
import brbo.common.MyLogger
import brbo.common.ast.BrboProgram
import brbo.common.commandline.FuzzingArguments

object Executor {
  val OUTPUT_DIRECTORY = s"${System.getProperty("user.dir")}/src/main/java/brbo/fuzz"

  def run(program: BrboProgram, arguments: FuzzingArguments): Unit = {
    val logger = MyLogger.createLogger(Executor.getClass, debugMode = arguments.getDebugMode)

    logger.info(s"Step 1: Prepare a QFuzz driver")
    val driverFileContents = DriverGenerator.run(program)
    val driverFilePath = s"$OUTPUT_DIRECTORY/${DriverGenerator.driverName(program.className)}.java"
    logger.info(s"Step 1: Write into $driverFilePath")
    BrboMain.writeToFile(driverFilePath, driverFileContents)

    logger.info(s"Step 2: Compile and instrument the QFuzz driver")
    logger.info(s"Step 3: Run QFuzz")
    logger.info(s"Step 4: Prepare inputs of interest")
  }
}

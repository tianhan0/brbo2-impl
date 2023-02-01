package brbo.backend2.qfuzz

import brbo.BrboMain
import brbo.backend2.InputParser
import brbo.backend2.qfuzz.DriverGenerator.{GeneratorParameters, parametersInLoopConditionals}
import brbo.common.ast._
import brbo.common.commandline.FuzzingArguments
import brbo.common.{BrboType, MyLogger}
import org.apache.commons.io.{FileUtils, FilenameUtils}
import play.api.libs.json.{JsArray, Json}

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.nio.{BufferUnderflowException, ByteBuffer}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Executor {
  private val DRIVERS_PATH = s"${System.getProperty("user.dir")}/src/main/java/brbo/fuzz/drivers"

  def run(program: BrboProgram, sourceFilePath: String, arguments: FuzzingArguments, generatorParameters: GeneratorParameters): Unit = {
    val logger = MyLogger.createLogger(Executor.getClass, debugMode = arguments.getDebugMode)

    val QFUZZ_PATH = arguments.getQFuzzPath
    val GUAVA_JAR_PATH = s"${System.getProperty("user.dir")}/lib/deps/guava-31.1-jre.jar"
    val KELINCI_JAR_PATH = s"$QFUZZ_PATH/tool/instrumentor/build/libs/kelinci.jar"
    val AFL_PATH = s"$QFUZZ_PATH/tool/afl-2.51b-qfuzz/afl-fuzz"
    val INTERFACE_C_PATH = s"$QFUZZ_PATH/tool/fuzzerside/interface"

    val AFL_TIMEOUT_IN_SECONDS = arguments.getAflTimeoutInSeconds
    val KELINCI_TIMEOUT_IN_SECONDS = AFL_TIMEOUT_IN_SECONDS + 3

    val OUTPUT_DIRECTORY: Path = Paths.get(arguments.getOutputPath)
    val FUZZ_OUT_DIRECTORY = s"$OUTPUT_DIRECTORY/fuzzer-out"
    val BINARY_PATH = s"$OUTPUT_DIRECTORY/bin"
    val INSTRUMENTED_BINARY_PATH = s"$OUTPUT_DIRECTORY/bin-instr"

    val INPUT_SEED_DIRECTORY = arguments.getInputDirectory

    logger.info(s"Step 0: Remove existing Json files")
    removeExistingInputs(sourceFilePath)
    logger.info(s"Step 1: Prepare a QFuzz driver. Naive mode? ${arguments.getNaive}")
    val driverFileContents = DriverGenerator.run(
      program = program,
      generatorParameters = generatorParameters,
      mode = if (arguments.getNaive) DriverGenerator.Naive else DriverGenerator.Modified
    )
    val driverFilePath = {
      Files.createDirectories(Paths.get(DRIVERS_PATH))
      val driverClassName = DriverGenerator.driverClassName(program.className)
      s"$DRIVERS_PATH/$driverClassName.java"
    }
    BrboMain.writeToFile(driverFilePath, driverFileContents)
    logger.info(s"Step 1: Written into `$driverFilePath`")

    logger.info(s"Step 2: Compile and instrument the QFuzz driver")
    logger.info(s"Step 2.1: Compile the QFuzz driver")
    prepareDirectory(BINARY_PATH)
    BrboMain.executeCommandWithLogger(
      // IMPORTANT: Avoid using quotes when specifying class path
      // To debug, add "-verbose" option and check the class paths
      command = s"""javac -cp .:$KELINCI_JAR_PATH:$GUAVA_JAR_PATH $driverFilePath -d $BINARY_PATH""",
      logger
    )
    logger.info(s"Step 2.2: Instrument the QFuzz driver")
    prepareDirectory(INSTRUMENTED_BINARY_PATH)
    BrboMain.executeCommandWithLogger(
      command = s"""java -cp .:$KELINCI_JAR_PATH:$GUAVA_JAR_PATH edu.cmu.sv.kelinci.instrumentor.Instrumentor -mode LABELS -i $BINARY_PATH -o $INSTRUMENTED_BINARY_PATH -skipmain""",
      logger
    )

    val commands = List("kelinci", "afl")
    val future = Future.traverse(commands)({
      case "kelinci" => Future {
        logger.info(s"Step 3: Run Kelinci server. Timeout: $KELINCI_TIMEOUT_IN_SECONDS sec.")
        val driverFullyQualifiedClassName = DriverGenerator.driverFullyQualifiedClassName(program.className)
        val command = s"""timeout --kill-after ${KELINCI_TIMEOUT_IN_SECONDS}s ${KELINCI_TIMEOUT_IN_SECONDS}s java -cp .:$KELINCI_JAR_PATH:$GUAVA_JAR_PATH:$INSTRUMENTED_BINARY_PATH edu.cmu.sv.kelinci.Kelinci -K 100 $driverFullyQualifiedClassName @@"""
        logger.info(s"Execute `$command`")
        val kelinciOutput = BrboMain.executeCommand(command = command, timeout = KELINCI_TIMEOUT_IN_SECONDS)
        logger.info(s"Kelinci log (truncated):\n${kelinciOutput.slice(from = kelinciOutput.length - 3000, until = kelinciOutput.length)}")
        val outputDirectory = s"${BrboMain.OUTPUT_DIRECTORY}/fuzz"
        Files.createDirectories(Paths.get(outputDirectory))
        BrboMain.writeToFile(s"$outputDirectory/kelinci_output.txt", kelinciOutput)
      }
      case "afl" => Future {
        logger.info(s"Step 4: Run AFL. Output directory: $FUZZ_OUT_DIRECTORY. Timeout: $AFL_TIMEOUT_IN_SECONDS sec.")
        // Ensure kelinci server starts first
        // Remember to use https://github.com/litho17/qfuzz/commit/5dbe56efc6a9d9f77c320f1a8f801f6c4d6400e3
        BrboMain.executeCommandWithLogger(command = "sleep 3", logger)
        val timeout = Duration(length = AFL_TIMEOUT_IN_SECONDS, unit = TimeUnit.SECONDS).toMillis
        BrboMain.executeCommandWithLogger(
          command = s"""timeout --kill-after ${AFL_TIMEOUT_IN_SECONDS}s ${AFL_TIMEOUT_IN_SECONDS}s $AFL_PATH -i $INPUT_SEED_DIRECTORY -o $FUZZ_OUT_DIRECTORY -c quantify -K 100 -S afl -t $timeout $INTERFACE_C_PATH -K 100 @@""",
          logger,
          environment = Map(
            "AFL_SKIP_CPUFREQ" -> "1",
            "AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES" -> "1",
          ),
          timeout = AFL_TIMEOUT_IN_SECONDS
        )
      }
    })
    Await.result(future, Duration.Inf)
    logger.info(s"Running Java processes:\n${runningJavaProcesses()}")
    logger.info(s"Kill kelinci servers")
    killRunningKelinciProcess()
    logger.info(s"Running Java processes:\n${runningJavaProcesses()}")
    val pathCostFileContents = BrboMain.readFromFile(s"$FUZZ_OUT_DIRECTORY/afl/path_costs.csv")
    logger.info(s"QFuzz output:\n$pathCostFileContents")
    val rankedInputFiles = {
      val generatedInputFiles = rankedInputFileNames(pathCostFileContents)
      if (generatedInputFiles.isEmpty)
        logger.warn(s"No interesting inputs. Using the input seed.")
      generatedInputFiles ::: getInputSeeds(INPUT_SEED_DIRECTORY)
    }
    logger.info(s"Ranked input files:\n${rankedInputFiles.mkString("\n")}")
    logger.warn(s"Must ensure the input seed provide sufficient amounts of data to be parsed as inputs")

    logger.info(s"Step 5: Parse the QFuzz-generated inputs")
    val listOfInputs: Iterable[List[Int]] = rankedInputFiles.map({
      inputFile =>
        val absolutePath = inputFile
        logger.info(s"Read shorts that are between [${arguments.getMinInteger}, ${arguments.getMaxInteger}] from $absolutePath")
        val shorts = parseBytesToShorts(absolutePath)
        val inputs = wrapShorts(shorts = shorts, max = generatorParameters.maxInteger, min = generatorParameters.minInteger)
        logger.info(s"Inputs: $inputs")
        inputs
    })
    val inputFilePath: String = freshInputFilePath(sourceFilePath)
    val listOfInputValues = listOfInputs.flatMap({
      inputs =>
        toInputValues(
          inputArray = inputs,
          parameters = program.mainFunction.parameters,
          parametersInLoopConditions = parametersInLoopConditionals(program.mainFunction),
          generatorParameters = generatorParameters)
    }).toList
    if (arguments.getDryRun) {
      logger.info(s"Step 6: Dry run. Not write interesting inputs into Json files")
    } else {
      if (listOfInputValues.nonEmpty) {
        logger.info(s"Step 6: Write interesting inputs (with descending interestingness) into file $inputFilePath")
        val allInputsJson = JsArray(
          listOfInputValues.map({
            inputs =>
              logger.info(s"Interesting input: ${inputs.map(v => v.printToIR()).mkString(", ")}")
              JsArray(inputs.map(input => InputParser.toJson(input)))
          })
        )
        BrboMain.writeToFile(path = inputFilePath, content = Json.prettyPrint(allInputsJson))
      } else {
        logger.info(s"Step 6: No interesting inputs")
      }
    }

    logger.info(s"Step 7: Clean up the generated files when running QFuzz")
    FileUtils.deleteDirectory(new File(FUZZ_OUT_DIRECTORY))
    FileUtils.deleteDirectory(new File(BINARY_PATH))
    FileUtils.deleteDirectory(new File(INSTRUMENTED_BINARY_PATH))
  }

  private def runningProcesses(): List[String] =
    BrboMain.executeCommand(command = "ps -ef").split("\n").toList

  private def runningJavaProcesses(): String = {
    runningProcesses().filter(process => process.contains("java")).mkString("\n")
  }

  private def killRunningKelinciProcess(): Unit = {
    val kelinciProcesses: List[String] =
      runningProcesses()
        .filter(process => process.contains("kelinci.jar"))
        .flatMap({
          line =>
            val symbols = line.split(" ")
            symbols.find(symbol => symbol.matches("[0-9]+"))
        })
    kelinciProcesses.foreach({
      pid => BrboMain.executeCommand(command = s"kill $pid")
    })
  }

  private def getInputSeeds(seed_directory: String): List[String] = {
    FileUtils.listFiles(new File(seed_directory), null, true).asScala
      .toList.map(file => file.getAbsolutePath)
  }

  private def deduplicate(inputs: List[List[BrboValue]]): List[List[BrboValue]] = {
    def toString(list: List[BrboValue]): String = list.map(v => v.printToIR()).mkString(", ")

    inputs.map({ input => (toString(input), input) }).toMap.values
      .toList.sortWith({ case (list1, list2) => toString(list1) < toString(list2) })
  }

  private def rankedInputFileNames(pathCostFileContents: String): List[String] = {
    val lines = pathCostFileContents.split("\n").filter({
      line => line != "Time(sec); File; #Partitions; MinDelta; AvgPartitionValues"
    })
    val inputFiles = lines.map({
      line =>
        var score = 0
        // Inputs that create new partitions w.r.t. the total runtime "cost"
        if (line.contains("+partition"))
          score += 8
        // Inputs that have the same number of partition as the max partition seen so far, but the distance between partitions is larger
        if (line.contains("+delta"))
          score += 4
        // Inputs that lead to a new edge in the control flow graph (i.e., it increases the edge coverage)
        if (line.contains("+cov"))
          score += 2
        val fileName = line.split(";")(1).trim
        (fileName, score)
    }).sortWith({
      case ((fileName1, score1), (fileName2, score2)) =>
        // Higher score means more desirability
        // Larger file name means the input is newer. Newer inputs seem more likely to vary
        score1 > score2 || fileName1 > fileName2
    }).map({
      case (fileName, _) => fileName
    })
    inputFiles.filter(fileName => !fileName.contains("orig:")).toList
  }

  private def readExistingInputs(inputFilePath: String): List[List[BrboValue]] = {
    if (Files.exists(Paths.get(inputFilePath))) {
      val inputFileContents = BrboMain.readFromFile(inputFilePath)
      InputParser.parse(inputFileContents)
    } else
      Nil
  }

  def getInputFilePath(sourceFilePath: String): String = {
    assert(FilenameUtils.getExtension(sourceFilePath) == "java")
    val directory = inputFileDirectory(sourceFilePath)
    val jsonFiles =
      FileUtils.listFiles(new File(directory), Array("json"), true).asScala
        .map({
          jsonFile =>
            println(s"Input file: ${jsonFile.getAbsolutePath}")
            jsonFile.getAbsolutePath
        })
        .toList
        .sorted
    jsonFiles.last
  }

  private def removeExistingInputs(sourceFilePath: String): Unit = {
    val directory = inputFileDirectory(sourceFilePath)
    FileUtils.listFiles(new File(directory), Array("json"), true)
      .asScala
      .foreach(file => Files.deleteIfExists(file.toPath))
  }

  private def freshInputFilePath(sourceFilePath: String): String = {
    val formatter = DateTimeFormatter.ofPattern("uuuuMMdd_HHmmss")
    val now = LocalDateTime.now
    val directory = inputFileDirectory(sourceFilePath)
    s"$directory/${inputFilePrefix(sourceFilePath)}_${formatter.format(now)}_qfuzz.json"
  }

  private def inputFileDirectory(sourceFilePath: String): String = {
    val basename = FilenameUtils.getBaseName(sourceFilePath).toLowerCase
    val directory = Paths.get(s"${FilenameUtils.getFullPath(sourceFilePath)}/$basename/qfuzz/")
    directory.toFile.mkdirs()
    directory.toString
  }

  private def inputFilePrefix(sourceFilePath: String): String =
    s"${FilenameUtils.getBaseName(sourceFilePath)}"

  def toInputValues(inputArray: List[Int],
                    parameters: List[Identifier],
                    parametersInLoopConditions: List[Identifier],
                    generatorParameters: GeneratorParameters): Option[List[BrboValue]] = {
    try {
      val (_, inputValues) = parameters.foldLeft(0, Nil: List[BrboValue])({
        case ((indexSoFar, inputValues), parameter) =>
          // Keep in sync with the initializations of input values in DriverGenerator.declarationsAndInitializations
          parameter.typ match {
            case BrboType.INT =>
              val number = inputArray(indexSoFar)
              val actualNumber =
                if (parametersInLoopConditions.exists(p => p.sameAs(parameter))) {
                  // Keep in sync with the generated drivers
                  number % (generatorParameters.maxLoopIterations - generatorParameters.minLoopIterations) + generatorParameters.minLoopIterations
                }
                else number
              (indexSoFar + 1, Number(actualNumber) :: inputValues)
            case BrboType.BOOL =>
              (indexSoFar + 1, Bool(inputArray(indexSoFar) > DriverGenerator.HALF_MAX_VALUE) :: inputValues)
            case BrboType.ARRAY(BrboType.INT) =>
              val arrayValue: List[Int] = inputArray.slice(indexSoFar, indexSoFar + generatorParameters.arraySize)
              val inputValue = BrboArray(values = arrayValue.map(v => Number(v)), innerType = BrboType.INT)
              (indexSoFar + generatorParameters.arraySize, inputValue :: inputValues)
            case _ => throw new Exception(s"Not support a parameter of type ${parameter.typ}")
          }
      })
      Some(inputValues.reverse)
    } catch {
      case _: IndexOutOfBoundsException =>
        // Sometimes insufficient inputs may be considered as interesting
        None
    }
  }

  private def prepareDirectory(directory: String): Unit = {
    val path = Paths.get(directory)
    if (Files.exists(path))
      FileUtils.deleteDirectory(new File(directory))
    Files.createDirectory(path)
  }

  private def parseBytesToShorts(path: String): List[java.lang.Short] = {
    val byteArray: Array[Byte] = Files.readAllBytes(Paths.get(path))
    byteArray.toList.grouped(java.lang.Short.BYTES).flatMap({
      short: List[Byte] =>
        try {
          val value: java.lang.Short = ByteBuffer.wrap(short.toArray).getShort()
          Some(value)
        } catch {
          case _: BufferUnderflowException => None
        }
    }).toList
  }

  private def wrapShorts(shorts: List[java.lang.Short], max: Int, min: Int): List[Int] = {
    shorts.map({
      short =>
        var result: Int = if (short >= 0) short.toInt else -short
        result = result % (max - min + 1) + min
        result
    })
  }
}

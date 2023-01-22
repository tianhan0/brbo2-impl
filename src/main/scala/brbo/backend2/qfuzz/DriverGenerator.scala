package brbo.backend2.qfuzz

import brbo.common.BrboType
import brbo.common.BrboType.QFuzzPrintType
import brbo.common.ast.PrintStyle.QFuzzJavaStyle
import brbo.common.ast.{BrboAstUtils, BrboFunction, BrboProgram, Identifier}
import brbo.common.string.StringFormatUtils
import brbo.frontend.TargetProgram

object DriverGenerator {
  val ARRAY_SIZE = 7
  /**
   * A too large number may result in executing the target program for too long. For example, an array input may
   * contain a large number that controls the loop iterations.
   * A too small number may result in a higher likelihood that the generated inputs do not very much.
   */
  val MAX_INTEGER = 20
  val MIN_INTEGER = 4
  val HALF_MAX_VALUE: Int = java.lang.Short.MAX_VALUE / 2
  val MIN_LOOP_ITERATIONS = 2
  val MAX_LOOP_ITERATIONS = 3
  val LOOP_ITERATION_MULTIPLIER = 10
  private val MAX_NUMBER_OF_USES_TO_TRACK = 1000

  case class GeneratorParameters(arraySize: Int,
                                 minInteger: Int,
                                 maxInteger: Int,
                                 minLoopIterations: Int,
                                 maxLoopIterations: Int,
                                 loopIterationMultiplier: Int)

  object GeneratorParameters {
    val default: GeneratorParameters = GeneratorParameters(
      arraySize = ARRAY_SIZE,
      minInteger = MIN_INTEGER,
      maxInteger = MAX_INTEGER,
      minLoopIterations = MIN_LOOP_ITERATIONS,
      maxLoopIterations = MAX_LOOP_ITERATIONS,
      loopIterationMultiplier = LOOP_ITERATION_MULTIPLIER,
    )
  }

  abstract class Mode

  object Modified extends Mode

  object Naive extends Mode

  def run(program: BrboProgram, generatorParameters: GeneratorParameters, mode: Mode): String = {
    val (declarations, initializations, prints) = declarationsAndInitializations(
      parameters = program.mainFunction.parameters,
      parametersInLoopConditions = parametersInLoopConditionals(program.mainFunction),
      generatorParameters = generatorParameters
    )
    val transformedProgram = ProgramTransformer.transform(
      program = program,
      loopIterationMultiplier = generatorParameters.loopIterationMultiplier
    )
    val resetActualObservations = s"${
      mode match {
        case Modified => ""
        case Naive => "actualObservations = new long[useCount];"
        case _ => throw new Exception
      }
    }"
    s"""package $DRIVER_PACKAGE_NAME;
       |
       |import edu.cmu.sv.kelinci.Kelinci;
       |import edu.cmu.sv.kelinci.Mem;
       |import edu.cmu.sv.kelinci.quantification.Greedy;
       |import edu.cmu.sv.kelinci.quantification.PartitionAlgorithm;
       |import edu.cmu.sv.kelinci.quantification.PartitionSet;
       |
       |import java.io.FileInputStream;
       |import java.io.IOException;
       |import java.nio.ByteBuffer;
       |import java.util.ArrayList;
       |import java.util.Arrays;
       |import java.util.List;
       |
       |public class ${driverClassName(program.className)} {
       |  public final static int ARRAY_SIZE = ${generatorParameters.arraySize};
       |  private final static short MAX_INTEGER = ${generatorParameters.maxInteger};
       |  private final static short MIN_INTEGER = ${generatorParameters.minInteger};
       |  private final static short MAX_LOOP_ITERATIONS = ${generatorParameters.maxLoopIterations};
       |  private final static short MIN_LOOP_ITERATIONS = ${generatorParameters.minLoopIterations};
       |  private final static int MAX_NUMBER_OF_USES_TO_TRACK = $MAX_NUMBER_OF_USES_TO_TRACK;
       |
       |  /* Minimum distance between clusters. */
       |  public final static double epsilon = 1.0;
       |
       |  /* Cluster Algorithm */
       |  public static PartitionAlgorithm clusterAlgorithm = new Greedy(false);
       |
       |  public static void main(String[] arguments) {
       |    if (arguments.length != 1) {
       |      System.out.println("Expects file name as parameter");
       |      return;
       |    }
       |
       |    List<Short> values = new ArrayList<>();
       |    try (FileInputStream inputStream = new FileInputStream(arguments[0])) {
       |      System.out.printf("Read shorts between [%d, %d]\\n", MIN_INTEGER, MAX_INTEGER);
       |      byte[] bytes = new byte[Short.BYTES];
       |      while ((inputStream.read(bytes) != -1)) {
       |        short rawValue = ByteBuffer.wrap(bytes).getShort();
       |        if (rawValue == Short.MIN_VALUE)
       |          continue;
       |        short value = rawValue >= 0 ? rawValue : (short) -rawValue;
       |        value = (short) (value % (MAX_INTEGER - MIN_INTEGER + 1) + MIN_INTEGER);
       |        System.out.printf("Read value: %d (raw: %d)\\n", value, rawValue);
       |        values.add(value);
       |      }
       |    } catch (IOException e) {
       |      System.err.println("Error reading input");
       |      e.printStackTrace();
       |      return;
       |    }
       |
       |${prependIndents(declarations, indent = 4).mkString("\n")}
       |    try {
       |${prependIndents(initializations, indent = 6).mkString("\n")}
       |    } catch (IndexOutOfBoundsException exception) {
       |      long[] actualObservations = new long[0];
       |      PartitionSet clusters = PartitionSet.createFromObservations(epsilon, actualObservations, clusterAlgorithm);
       |      Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
       |      System.out.println("Not sufficient inputs!");
       |      return;
       |    }
       |${prependIndents(prints, indent = 4).mkString("\n")}
       |
       |    long[] observations = new long[MAX_NUMBER_OF_USES_TO_TRACK];
       |    ${program.className} program = new ${program.className}();
       |    Mem.clear(true);
       |    int useCount = 0;
       |    long lastMemInstrCost = 0L;
       |    int iThUse = 1;
       |    for (; iThUse <= MAX_NUMBER_OF_USES_TO_TRACK; iThUse++) {
       |      // In the i-th iteration, we collect accumulated resource consumption up to the secret[i]-th uses
       |      Mem.clear(false);
       |      useCount = program.${TargetProgram.MAIN_FUNCTION}(${program.mainFunction.parameters.map({ identifier => identifier.name }).mkString(", ")}, iThUse);
       |      if (iThUse <= useCount)
       |        System.out.printf("iThUse: %d; cost: %d; useCount: %d\\n", iThUse, Mem.instrCost, useCount);
       |      long thisCost = 0L;
       |      if (iThUse == 1) {
       |        thisCost = Mem.instrCost;
       |      } else {
       |        thisCost = Mem.instrCost - lastMemInstrCost;
       |      }
       |      assert (thisCost >= 0);
       |      observations[iThUse - 1] = thisCost;
       |      lastMemInstrCost = Mem.instrCost;
       |    }
       |    long[] actualObservations = new long[useCount];
       |    System.arraycopy(observations, 0, actualObservations, 0, actualObservations.length);
       |    System.out.println("observations: " + Arrays.toString(actualObservations));
       |
       |    $resetActualObservations
       |    PartitionSet clusters = PartitionSet.createFromObservations(epsilon, actualObservations, clusterAlgorithm);
       |    // Give feedback to fuzzer: Number of clusters, min distance between clusters
       |    Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
       |
       |    System.out.println("Done.");
       |  }
       |}
       |
       |${transformedProgram.print(indent = 0, style = QFuzzJavaStyle)}""".stripMargin
  }

  def driverClassName(className: String): String = s"${className}QFuzzDriver"

  def driverFullyQualifiedClassName(className: String): String = s"$DRIVER_PACKAGE_NAME.${driverClassName(className)}"

  private val DRIVER_PACKAGE_NAME = "brbo.fuzz.drivers"

  def prependIndents(lines: List[String], indent: Int): List[String] =
    lines.map(line => StringFormatUtils.prependIndentsPerLine(line, indent))

  def declarationsAndInitializations(parameters: List[Identifier],
                                     parametersInLoopConditions: List[Identifier],
                                     generatorParameters: GeneratorParameters): (List[String], List[String], List[String]) = {
    val (_, declarations, initializations, prints) = parameters.foldLeft((0, Nil: List[String], Nil: List[String], Nil: List[String]))({
      case ((indexSoFar, declarations, initializations, prints), parameter) =>
        parameter.typ match {
          case BrboType.INT =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)};"
            val initialization = s"${parameter.name} = values.get($indexSoFar);"
            val wrap =
              if (parametersInLoopConditions.exists(p => p.sameAs(parameter))) {
                // If a variable is used in loop conditions, then it must benot in [MIN_LOOP_ITERATIONS, MAX_LOOP_ITERATIONS],
                // to avoid executing a loop for too many (which makes it expensive to choose segments) or too few times (which ???).
                s"${parameter.name} = ${parameter.name} % (MAX_LOOP_ITERATIONS - MIN_LOOP_ITERATIONS + 1) + MIN_LOOP_ITERATIONS;"
              }
              else ""
            val print = s"""System.out.println("${parameter.name}: " + ${parameter.name});"""
            (indexSoFar + 1,
              declaration :: declarations,
              wrap :: initialization :: initializations,
              print :: prints)
          case BrboType.ARRAY(BrboType.INT) =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)} = new int[ARRAY_SIZE];"
            val initialization =
              s"""for (int i = 0; i < ARRAY_SIZE && $indexSoFar + i < values.size(); i++) {
                 |  ${parameter.name}[i] = values.get($indexSoFar + i);
                 |}""".stripMargin
            val print = s"""System.out.println("${parameter.name}: " + Arrays.toString(${parameter.name}));"""
            (indexSoFar + generatorParameters.arraySize,
              declaration :: declarations,
              initialization :: initializations,
              print :: prints)
          case BrboType.BOOL =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)};"
            val initialization = s"${parameter.name} = values.get($indexSoFar) > $HALF_MAX_VALUE;"
            val print = s"""System.out.println("${parameter.name}: " + ${parameter.name});"""
            (indexSoFar + 1,
              declaration :: declarations,
              initialization :: initializations,
              print :: prints)
          case _ => throw new Exception(s"Not support a parameter of type ${parameter.typ}")
        }
    })
    (declarations.reverse, initializations.reverse, prints.reverse)
  }

  def parametersInLoopConditionals(function: BrboFunction): List[Identifier] = {
    val loopConditionals = BrboAstUtils.getLoopConditionals(function.body)
    function.parameters.filter({
      identifier => identifier.typ == BrboType.INT && loopConditionals.exists({ e => e.getUses.contains(identifier) })
    })
  }
}

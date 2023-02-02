package brbo.backend2.qfuzz

import brbo.backend2.qfuzz.DriverGenerator.GeneratorParameters
import brbo.backend2.qfuzz.DriverGeneratorUnitTest.test01
import brbo.common.BrboType
import brbo.common.ast.Identifier
import brbo.common.string.StringCompare
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.scalatest.flatspec.AnyFlatSpec

class DriverGeneratorUnitTest extends AnyFlatSpec {
  "Generating declarations and initializations of parameters" should "be correct" in {
    val parameters = List(
      Identifier("a", BrboType.INT),
      Identifier("b", BrboType.ARRAY(BrboType.INT)),
      Identifier("c", BrboType.ARRAY(BrboType.INT)),
      Identifier("d", BrboType.BOOL),
      Identifier("e", BrboType.INT),
    )
    val (declarations, initializations, prints) = DriverGenerator.declarationsAndInitializations(
      parameters = parameters,
      parametersInLoopConditions = List(Identifier("e", BrboType.INT)),
      generatorParameters = GeneratorParameters(arraySize = 5, minInteger = 4, maxInteger = 200, minLoopIterations = 2, maxLoopIterations = 3, loopIterationMultiplier = 1)
    )
    val result = (declarations ::: initializations ::: prints).mkString("\n")
    StringCompare.ignoreWhitespaces(result,
      """int a;
        |int[] b = new int[ARRAY_SIZE];
        |int[] c = new int[ARRAY_SIZE];
        |boolean d;
        |int e;
        |a = values.get(0);
        |
        |for (int i = 0; i < ARRAY_SIZE && 1 + i < values.size(); i++) {
        |  b[i] = values.get(1 + i);
        |}
        |for (int i = 0; i < ARRAY_SIZE && 6 + i < values.size(); i++) {
        |  c[i] = values.get(6 + i);
        |}
        |d = values.get(11) > 16383;
        |e = values.get(12);
        |e = e % (MAX_LOOP_ITERATIONS - MIN_LOOP_ITERATIONS + 1) + MIN_LOOP_ITERATIONS;
        |System.out.println("a: " + a);
        |System.out.println("b: " + Arrays.toString(b));
        |System.out.println("c: " + Arrays.toString(c));
        |System.out.println("d: " + d);
        |System.out.println("e: " + e);""".stripMargin)
  }

  "Generating a QFuzz driver" should "be correct" in {
    val targetProgram = BasicProcessor.getTargetProgram("Test", test01)
    val result = DriverGenerator.run(
      program = targetProgram.program,
      generatorParameters = GeneratorParameters(arraySize = 5, minInteger = 4, maxInteger = 200, minLoopIterations = 2, maxLoopIterations = 3, loopIterationMultiplier = 1),
      mode = DriverGenerator.Modified
    )
    StringCompare.ignoreWhitespaces(result,
      """package brbo.fuzz.drivers;
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
        |import java.util.HashSet;
        |import java.util.Set;
        |import com.google.common.collect.Sets;
        |
        |public class TestQFuzzDriver {
        |  public final static int ARRAY_SIZE = 5;
        |  private final static short MAX_INTEGER = 200;
        |  private final static short MIN_INTEGER = 4;
        |  private final static short MAX_LOOP_ITERATIONS = 3;
        |  private final static short MIN_LOOP_ITERATIONS = 2;
        |  private final static int MAX_NUMBER_OF_USES_TO_TRACK = 1000;
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
        |      System.out.printf("Read shorts between [%d, %d]\n", MIN_INTEGER, MAX_INTEGER);
        |      byte[] bytes = new byte[Short.BYTES];
        |      while ((inputStream.read(bytes) != -1)) {
        |        short rawValue = ByteBuffer.wrap(bytes).getShort();
        |        if (rawValue == Short.MIN_VALUE)
        |          continue;
        |        short value = rawValue >= 0 ? rawValue : (short) -rawValue;
        |        value = (short) (value % (MAX_INTEGER - MIN_INTEGER + 1) + MIN_INTEGER);
        |        System.out.printf("Read value: %d (raw: %d)\n", value, rawValue);
        |        values.add(value);
        |      }
        |    } catch (IOException e) {
        |      System.err.println("Error reading input");
        |      e.printStackTrace();
        |      return;
        |    }
        |
        |    int a;
        |    int[] array = new int[ARRAY_SIZE];
        |    int b;
        |    boolean c;
        |    try {
        |      a = values.get(0);
        |
        |      for (int i = 0; i < ARRAY_SIZE && 1 + i < values.size(); i++) {
        |        array[i] = values.get(1 + i);
        |      }
        |      b = values.get(6);
        |
        |      c = values.get(7) > 16383;
        |    } catch (IndexOutOfBoundsException exception) {
        |      long[] actualObservations = new long[0];
        |      PartitionSet clusters = PartitionSet.createFromObservations(epsilon, actualObservations, clusterAlgorithm);
        |      Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
        |      System.out.println("Not sufficient inputs!");
        |      return;
        |    }
        |    System.out.println("a: " + a);
        |    System.out.println("array: " + Arrays.toString(array));
        |    System.out.println("b: " + b);
        |    System.out.println("c: " + c);
        |
        |    long[] observations = new long[MAX_NUMBER_OF_USES_TO_TRACK];
        |    Test program = new Test();
        |    Mem.clear(true);
        |    int DEFAULT_USE_COST = 0;
        |    int useCost = DEFAULT_USE_COST;
        |    for (int iThUse = 1; iThUse <= MAX_NUMBER_OF_USES_TO_TRACK; iThUse++) {
        |      // In the i-th iteration, we collect accumulated resource consumption up to the secret[i]-th uses
        |      Mem.clear(false);
        |      // Instead of relying on the instrumentation for measuring the use costs, we directly count them.
        |      useCost = program.execute(a, array, b, c, iThUse);
        |      if (useCost != DEFAULT_USE_COST)
        |        System.out.printf("iThUse: %d; cost: %d; useCost: %d\n", iThUse, Mem.instrCost, useCost);
        |      observations[iThUse - 1] = useCost;
        |    }
        |    List<Long> observationList = new ArrayList<>();
        |    for (long observation: observations) {
        |      if (observation != DEFAULT_USE_COST)
        |        observationList.add(observation);
        |    }
        |    System.out.println("observations: " + Arrays.toString(observationList.toArray()));
        |
        |    // Find the sums of all segments
        |    Set<Long> segmentSums = new HashSet<>();
        |    List<Integer> indexList = new ArrayList<>();
        |    int INDEX = 0;
        |    for (long actualObservation: observationList) {
        |      indexList.add(INDEX);
        |      INDEX++;
        |    }
        |    int MAX_SEGMENT_LENGTH = ARRAY_SIZE; // TODO: Update this if the relation changes
        |    for (int size = 1; size <= MAX_SEGMENT_LENGTH && size <= indexList.size(); size++) {
        |      Set<Set<Integer>> subsets = Sets.combinations(new HashSet<>(indexList), size);
        |      for (Set<Integer> subset: subsets) {
        |        long sum = 0;
        |        for (Integer index: subset) {
        |          sum += observationList.get(index);
        |        }
        |        segmentSums.add(sum);
        |      }
        |    }
        |    long[] actualObservations = new long[segmentSums.size()];
        |    INDEX = 0;
        |    for (Long sum: segmentSums) {
        |      // Kelinci can only send these many observations to afl.
        |      if (INDEX >= 100) {
        |        break;
        |      }
        |      actualObservations[INDEX] = sum;
        |      INDEX++;
        |    }
        |    System.out.println("observations (sums): " + Arrays.toString(actualObservations));
        |
        |
        |    PartitionSet clusters = PartitionSet.createFromObservations(epsilon, actualObservations, clusterAlgorithm);
        |    // Give feedback to fuzzer: Number of clusters, min distance between clusters
        |    Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
        |
        |    System.out.println("Done.");
        |  }
        |}
        |
        |
        |class Test {
        |  int execute(int a, int[] array, int b, boolean c, int INDEX_VARIABLE)
        |  {
        |    int USE_COUNT = 0;
        |    int USE = 0;
        |    int x = arrayLength(array);
        |    x = arraySum(array);
        |    x = arrayRead(array, 3);
        |    // int R = 0;
        |    USE = 3;
        |    USE_COUNT = USE_COUNT + 1;
        |    if (INDEX_VARIABLE == USE_COUNT)
        |    {
        |      return USE;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    return 0;
        |  }
        |  int arrayRead(int[] array, int index) { return array[index]; }
        |  int arrayLength(int[] array) { return array.length; }
        |  int arraySum(int[] array) {
        |    int sum = 0;
        |    for (int element : array) {
        |      sum += element;
        |    }
        |    return sum;
        |  }
        |  void mostPreciseBound(boolean assertion) {}
        |  void lessPreciseBound(boolean assertion) {}
        |  boolean ndBool2(int... values) {
        |    int sum = 0;
        |    for (int value : values) {
        |      sum += value;
        |    }
        |    // mod 2 results in a higher chance of producing an alternative value, when compared with mod 3
        |    return sum % 2 == 0;
        |  }
        |  int ndInt2(int lower, int upper) {
        |    if (upper < lower)
        |      System.exit(-1);
        |    return upper > lower ? lower + 1 : upper;
        |  }
        |  void use(int n)
        |  {
        |    int i = 0;
        |    while (i < (n * 1))
        |    {
        |      i = i + 1;
        |    }
        |  }
        |}""".stripMargin)
  }
}

object DriverGeneratorUnitTest {
  private val test01 =
    s"""abstract class Test {
       |  void ${TargetProgram.MAIN_FUNCTION}(int a, int[] array, int b, boolean c) {
       |    int x = arrayLength(array);
       |    x = arraySum(array);
       |    x = arrayRead(array, 3);
       |    int R = 0;
       |    R = R + 3;
       |  }
       |  abstract int arrayRead(int[] x, int index);
       |  abstract int arraySum(int[] x);
       |  abstract int arrayLength(int[] x);
       |}""".stripMargin
}

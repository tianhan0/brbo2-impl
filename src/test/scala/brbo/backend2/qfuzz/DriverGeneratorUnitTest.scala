package brbo.backend2.qfuzz

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
      Identifier("d", BrboType.INT),
    )
    val (declarations, initializations) = DriverGenerator.declarationsAndInitializations(parameters)
    val result = (declarations ::: initializations).mkString("\n")
    StringCompare.ignoreWhitespaces(result,
      """int a = values.get(0);
        |int[] b = new int[ARRAY_SIZE];
        |int[] c = new int[ARRAY_SIZE];
        |int d = values.get(17);
        |for (int i = 0; i < ARRAY_SIZE && 1 + i < values.size(); i++) {
        |  b[i] = values.get(1 + i);
        |}
        |for (int i = 0; i < ARRAY_SIZE && 9 + i < values.size(); i++) {
        |  c[i] = values.get(9 + i);
        |}""".stripMargin)
  }

  "Generating a QFuzz driver" should "be correct" in {
    val targetProgram = BasicProcessor.getTargetProgram("Test", test01)
    val result = DriverGenerator.run(targetProgram.program)
    StringCompare.ignoreWhitespaces(result,
      """package brbo.fuzz;
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
        |public class TestQFuzzDriver {
        |  public final static int ARRAY_SIZE = 8;
        |  private final static short MAX_INTEGER = 30;
        |  private final static short MIN_INTEGER = 1;
        |  private final static int MAX_NUMBER_OF_USES_TO_TRACK = 1000;
        |
        |  /* Minimum distance between clusters. */
        |  public final static double epsilon = 1.0;
        |
        |  /* Cluster Algorithm */
        |  public static PartitionAlgorithm clusterAlgorithm = new Greedy(false);
        |
        |  public static void main(String[] args) {
        |    if (args.length != 1) {
        |      System.out.println("Expects file name as parameter");
        |      return;
        |    }
        |
        |    List<Short> values = new ArrayList<>();
        |    try (FileInputStream inputStream = new FileInputStream(args[0])) {
        |      System.out.printf("Reading shorts that are between [%d, %d]", MIN_INTEGER, MAX_INTEGER);
        |      byte[] bytes = new byte[Short.BYTES];
        |      while ((inputStream.read(bytes) != -1)) {
        |        short value = ByteBuffer.wrap(bytes).getShort();
        |        value = value >= 0 ? value : (short) -value;
        |        value = (short) (value % (MAX_INTEGER - MIN_INTEGER + 1) + MIN_INTEGER);
        |        System.out.println("Read value: " + value);
        |        values.add(value);
        |      }
        |    } catch (IOException e) {
        |      System.err.println("Error reading input");
        |      e.printStackTrace();
        |      return;
        |    }
        |
        |    int a = values.get(0);
        |    int[] array = new int[ARRAY_SIZE];
        |    int b = values.get(9);
        |    for (int i = 0; i < ARRAY_SIZE && 1 + i < values.size(); i++) {
        |      array[i] = values.get(1 + i);
        |    }
        |
        |    // System.out.println("public: " + Arrays.toString(public_input));
        |
        |    long[] observations = new long[MAX_NUMBER_OF_USES_TO_TRACK];
        |    Test program = new Test();
        |    Mem.clear(true);
        |    for (int iUse = 0; iUse < MAX_NUMBER_OF_USES_TO_TRACK; iUse++) {
        |      // In the i-th iteration, we collect accumulated resource consumption up to the secret[i]-th uses
        |      Mem.clear(false);
        |      program.execute(a, array, b, iUse);
        |      if (iUse == 0) {
        |        observations[iUse] = Mem.instrCost;
        |      } else {
        |        observations[iUse] = Mem.instrCost - observations[iUse - 1];
        |      }
        |    }
        |    System.out.println("observations: " + Arrays.toString(observations));
        |
        |    PartitionSet clusters = PartitionSet.createFromObservations(epsilon, observations, clusterAlgorithm);
        |    // Give feedback to fuzzer: Number of clusters, min distance between clusters
        |    Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
        |
        |    System.out.println("Done.");
        |  }
        |}
        |
        |
        |class Test {
        |  void execute(int a, int[] array, int b, int INDEX_VARIABLE)
        |  {
        |    int x = arrayLength(array);
        |    x = arraySum(array);
        |    x = arrayRead(array, 3);
        |    int R = 0;
        |    {
        |      use(3);
        |      INDEX_VARIABLE = INDEX_VARIABLE - 1;
        |      if (INDEX_VARIABLE == 0)
        |      {
        |        return;
        |      }
        |      else
        |      {
        |        ;
        |      }
        |    }
        |  }
        |  int arrayRead(int[] array, int index) { return array[index]; }
        |  int arrayLength(int[] array) { return array.length; }
        |  int arraySum(int[] array) {
        |    int sum = 0;
        |    for (int i = 0; i < array.length; i++) {
        |      sum += array[i];
        |    }
        |    return sum;
        |  }
        |  void mostPreciseBound(boolean assertion) {}
        |  void lessPreciseBound(boolean assertion) {}
        |  void use(int n)
        |  {
        |    int i = 0;
        |    while (i < n)
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
       |  void ${TargetProgram.MAIN_FUNCTION}(int a, int[] array, int b) {
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

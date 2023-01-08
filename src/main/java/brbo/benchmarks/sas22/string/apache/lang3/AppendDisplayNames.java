package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendDisplayNames extends Common {
  void execute(int[] sorted) {
    int R = 0;
    mostPreciseBound(R <= arraySum(sorted) + 2 * arraySum(sorted));
    lessPreciseBound(R <= MAX * arraySum(sorted) + MAX);
    int entry = 0;
    for (int i = 0; i < arrayLength(sorted); i++) {
      entry = arrayRead(sorted, i);
      for (int j = 0; j < entry; j++) {
        R = R + 2;
      }
      R = R + 1;
    }
  }
}

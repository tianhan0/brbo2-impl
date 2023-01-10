package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class GetFormattedExceptionMessage extends Common {
  void execute(int[] baseMessage) {
    if (arrayLength(baseMessage) <= 1) {
      return;
    }
    int buffer = 0;
    int R = 0;
    mostPreciseBound(R <= 3 + 6 * arraySum(baseMessage));
    lessPreciseBound(R <= MAX + MAX * arraySum(baseMessage));
    R  = R + 1;
    R = R + 1;
    int chunk = 0;
    for (int i = 0; i < arrayLength(baseMessage); i++) {
      chunk = arrayRead(baseMessage, i);
      // int key = ndInt2(1, entry);
      // int value = ndInt2(1, entry - key);
      R = R + 1;
      R = R + 1;
      R = R + 1;
      R = R + chunk;
      R = R + 1;
      // if (ndBool2(iterator)) {
        R = R + 1;
      // }
      /*else {
        buffer += value;
        R = R + value;
      }*/
      R = R + 1;
    }
    R = R + 1;
  }
}

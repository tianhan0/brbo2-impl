package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SplitByWholeSeparatorWorker extends Common {
  void execute(int[] str,
               int[] isSeparator,
               int max) {
    if (arraySum(str) <= 0 || max <= 0 || arrayLength(isSeparator) < arrayLength(str)) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(str));
    lessPreciseBound(R <= MAX * arraySum(str) + MAX);
    int numberOfStrings = 0;
    int chunk = 0;
    int isSeparatorChunk = 0;
    for (int i = 0; i < arrayLength(str); i++) {
      chunk = arrayRead(str, i);
      if (numberOfStrings >= max) {
        R = R + chunk;
        i++;
        continue;
      }
      isSeparatorChunk = arrayRead(isSeparator, i);
      if (isSeparatorChunk > BOOLEAN_SEPARATOR) {
        // This chunk is a separator
        numberOfStrings++;
      } else {
        // This chunk is not a separator
        R = R + chunk;
      }
    }
  }
}

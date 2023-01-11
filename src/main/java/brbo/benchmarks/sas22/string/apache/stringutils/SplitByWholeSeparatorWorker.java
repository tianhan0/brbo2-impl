package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SplitByWholeSeparatorWorker extends Common {
  void execute(int[] str,
               int isSeparator,
               int max) {
    if (arrayLength(str) <= 1 || max <= 0 || !(isSeparator >= 0 && isSeparator < arrayLength(str))) {
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
      if (isSeparatorChunk < i) {
        // This chunk is a separator
        numberOfStrings++;
      } else {
        // This chunk is not a separator
        R = R + chunk;
      }
    }
  }
}

package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class Replace extends Common {
  void execute(int[] text, int searchString, int replacement, int max) {
    if (arraySum(text) <= 0 || searchString <= 0 || max <= 0 || replacement <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(text));
    lessPreciseBound(R <= MAX * arraySum(text) + MAX);
    int chunk = 0;
    for (int i = 0; i < arrayLength(text);) {
      chunk = arrayRead(text, i);
      R = R + chunk;
      if (i + 1 < arrayLength(text)) {
        chunk = arrayRead(text, i + 1);
        // R = R + replacement
        max--;
        if (max == 0) {
          break;
        }
        i += 2;
      } else {
        i++;
      }
    }
  }
}

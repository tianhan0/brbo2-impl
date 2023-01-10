package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class Replace extends Common {
  void execute(int[] text, int searchString, int replacement, int max) {
    if (arrayLength(text) <= 1 || searchString <= 0 || max <= 0 || replacement <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(text) + arraySum(text) * replacement);
    lessPreciseBound(R <= MAX + MAX * arraySum(text) + MAX * arraySum(text) * replacement);
    int chunk = 0;
    int i = 0;
    while (i + 1 < arrayLength(text)) {
      chunk = arrayRead(text, i);
      R = R + chunk;
      R = R + replacement;
      // max--;
      // if (max == 0) {
        // TODO: The `break` statement makes the reset place holder at the end of the loop body
        //  no longer post-dominate the statements in the loop body
        // break;
      // }
      chunk = arrayRead(text, i + 1);
      i += 2;
    }
  }
}

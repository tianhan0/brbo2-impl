package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class Join extends Common {
  void execute(int[] array, int startIndex, int endIndex) {
    if (arraySum(array) <= 0 ||
        startIndex <= 0 || startIndex >= arrayLength(array) ||
        endIndex <= 0 || endIndex >= arrayLength(array) ||
        endIndex - startIndex <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(array) + arraySum(array));
    lessPreciseBound(R <= MAX + MAX * arraySum(array) + MAX * arraySum(array));
    int i = startIndex;
    int chunk = 0;
    chunk = arrayRead(array, i);
    R = R + chunk;
    i++;
    for (; i < endIndex; i++) {
      chunk = arrayRead(array, i);
      R = R + chunk;
      R = R + 1;
    }
  }
}

package brbo.benchmarks.sas22.sas21.string.guava;

import brbo.benchmarks.Common;

abstract public class LenientFormat extends Common {
  void main(int[] template, int args) {
    if (arraySum(template) <= 0 || args <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(template) + args + 2 + 2 * args);
    lessPreciseBound(R <= MAX * arraySum(template) + MAX * args + MAX);
    int chunk = 0;
    int separator = 0;
    int i = 0;
    chunk = arrayRead(template, i);
    R = R + chunk;
    i++;
    while (i + 1 < arrayLength(template)) {
      separator = arrayRead(template, i);
      // assume(separator == 2);
      chunk = arrayRead(template, i + 1);
      R = R + chunk;
      i += 2;
    }

    if (i < args) {
      R = R + 1;
      R = R + 1;
      i++;
      while (i < args) {
        R = R + 1;
        R = R + 1;
        i = i + 1;
      }
    }
  }
}

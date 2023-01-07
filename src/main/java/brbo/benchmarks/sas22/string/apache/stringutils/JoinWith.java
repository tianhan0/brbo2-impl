package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class JoinWith extends Common {
  void execute(int[] objects, int separator) {
    if (arrayLength(objects) <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(objects) + arraySum(objects) * separator);
    lessPreciseBound(R <= MAX + MAX * arraySum(objects) + MAX * arraySum(objects) * separator);
    int i = 0;
    int chunk = 0;
    chunk = arrayRead(objects, i);
    R = R + chunk;
    i++;
    for (; i < arrayLength(objects); i++) {
      chunk = arrayRead(objects, i);
      R = R + chunk;
      R = R + separator;
    }
  }
}

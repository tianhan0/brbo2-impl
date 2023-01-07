package brbo.benchmarks.sas22.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class Join2 extends Common {
  void execute(int[] objects) {
    if (arrayLength(objects) <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= arraySum(objects) + arraySum(objects));
    lessPreciseBound(R <= MAX + MAX * arraySum(objects) + MAX * arraySum(objects));
    int i = 0;
    int chunk = 0;
    chunk = arrayRead(objects, i);
    R = R + chunk;
    i++;
    for (; i < arrayLength(objects); i++) {
      chunk = arrayRead(objects, i);
      R = R + chunk;
      R = R + 1;
    }
  }
}

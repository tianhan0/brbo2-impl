package brbo.benchmarks.sas22.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendDetail extends Common {
  void main(int array) {
    if (array <= 0) {
      return;
    }
    int buffer = 0;
    int R = 0;
    mostPreciseBound(R <= 1 + array);
    lessPreciseBound(R <= MAX + MAX * array);
    for (int i = 0; i < array; i++) {
      if (i > 0) {
        buffer++;
        R = R + 1;
      }
      buffer++;
      R = R + 1;
    }
  }
}

package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class Literal extends Common {
  void execute(int pattern) {
    if (pattern <= 0) {
      return;
    }
    int sb = 0;
    int R = 0;
    mostPreciseBound(R <= pattern);
    lessPreciseBound(R <= MAX * pattern + MAX);
    int currentIdx = ndInt2(0, pattern - 1);
    while (currentIdx < pattern) {
      /*if (ndBool2(currentIdx)) {
        break;
      } else {
        if (ndBool2(currentIdx)) {
          continue;
        }
      }*/
      ++currentIdx;
      sb++;
      R = R + 1;
    }
  }
}

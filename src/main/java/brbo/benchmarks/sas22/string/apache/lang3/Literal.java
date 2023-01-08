package brbo.benchmarks.sas22.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class Literal extends Common {
  void execute(int pattern, int currentIdx) {
    if (pattern <= 0 || currentIdx <= 0 || currentIdx >= pattern - 1) {
      return;
    }
    int sb = 0;
    int R = 0;
    mostPreciseBound(R <= pattern);
    lessPreciseBound(R <= MAX * pattern + MAX);
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

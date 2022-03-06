package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class Literal extends Common {
  void main(int pattern) {
    if (pattern <= 0) {
      return;
    }
    int sb = 0;
    int R = 0;
    boundAssertion("most", R <= pattern);
    boundAssertion("less", R <= MAX * pattern + MAX);
    int currentIdx = ndInt2(0, pattern - 1);
    while (currentIdx < pattern) {
      if (ndInt() == 0) {
        break;
      } else {
        if (ndInt() == 0) {
          continue;
        }
      }
      ++currentIdx;
      sb++;
      R = R + 1;
    }
  }
}

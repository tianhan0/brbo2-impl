package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class DbgPrint extends Common {
  void main(int src) {
    if (src <= 0) {
      return;
    }
    int sb = 0;
    int R = 0;
    boundAssertion("most", R <= src);
    boundAssertion("less", R <= MAX * src + MAX);
    for (int i = 0; i < src; i++) {
      if (ndInt() == 0) {
        sb++;
        R = R + 1;
      }
      else {
        sb++;
        R = R + 1;
      }
    }
  }
}

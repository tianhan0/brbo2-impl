package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class SimpleQuote extends Common {
  void main(int value) {
    if (value <= 0) {
      return;
    }
    int sb = 0;
    int R = 0;
    boundAssertion("most", R <= value * 2 + 1);
    boundAssertion("less", R <= MAX * value + MAX);
    for (int i = 0; i < value; i++) {
      if (ndBool()) {
        sb += 2;
        R = R + 2;
      } else {
        sb++;
        R = R + 1;
      }
    }
    if (ndBool()) {
      sb++;
      R = R + 1;
    }
  }
}

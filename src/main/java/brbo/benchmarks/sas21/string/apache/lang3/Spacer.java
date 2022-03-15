package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class Spacer extends Common {
  void main(int spaces) {
    if (spaces <= 0) {
      return;
    }
    int sb = 0;
    int R = 0;
    boundAssertion("most", R <= spaces);
    boundAssertion("less", R <= MAX* spaces + MAX);
    for (int i = 0; i < spaces; i++) {
      sb++;
      R = R + 1;
    }
  }
}

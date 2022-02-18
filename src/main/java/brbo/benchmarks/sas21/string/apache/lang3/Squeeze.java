package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class Squeeze extends Common {
  void main(int str) {
    if (str <= 0) {
      return;
    }
    int buffer = 0;
    int R = 0;
    boundAssertion("most", R <= str);
    boundAssertion("less", R <= MAX * str + MAX);
    for (int i = 1; i < str; i++) {
      if (ndBool()) {
        if (ndBool()) {
          continue;
        }
        if (ndBool()) {
          if (ndBool()) {
            continue;
          }
        }
      }
      buffer++;
      R = R + 1;
    }
  }
}

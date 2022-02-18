package brbo.benchmarks.sas21.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class Modify extends Common {
  void main(int str) {
    if (str <= 0) {
      return;
    }
    int buffer = 0;
    int R = 0;
    boundAssertion("most", R <= str);
    boundAssertion("less", R <= MAX * str + MAX);
    int i = 0;
    while (i < str) {
      if (ndBool()) {
        buffer++;
        R = R + 1;
      }
      i++;
    }
  }
}

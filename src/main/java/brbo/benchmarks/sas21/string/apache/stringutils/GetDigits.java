package brbo.benchmarks.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class GetDigits extends Common {
  void main(int str) {
    if (str <= 0) {
      return;
    }
    int R = 0;
    boundAssertion("most", R <= str);
    boundAssertion("less", R <= MAX * str);
    int strDigits = 0;
    for (int i = 0; i < str; i++) {
      if (ndInt() == 0) {
        strDigits++;
        R = R + 1;
      }
    }
  }
}

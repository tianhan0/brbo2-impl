package brbo.benchmarks.sas22.sas21.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class GetDigits extends Common {
  void main(int str) {
    if (str <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= str);
    lessPreciseBound(R <= MAX * str);
    int strDigits = 0;
    for (int i = 0; i < str; i++) {
      if (ndBool()) {
        strDigits++;
        R = R + 1;
      }
    }
  }
}
